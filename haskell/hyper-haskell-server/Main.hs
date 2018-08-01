{-----------------------------------------------------------------------------
    HyperHaskell
------------------------------------------------------------------------------}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTSyntax, ExistentialQuantification, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Control.Monad.Catch
import Control.Exception             (AsyncException(UserInterrupt), evaluate)
import Data.List                     (groupBy)
import Data.Maybe                    (catMaybes)
import Data.Typeable
import Text.Read                     (readMaybe)
import System.Environment  as System

-- Haskell interpreter
import           Language.Haskell.Interpreter hiding (eval, setImports)
import qualified Language.Haskell.Interpreter as Hint

-- Haskell language parsing
import qualified Language.Haskell.Exts as Haskell

-- web
import           Data.Aeson                    (toJSON, (.=))
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as B
import Data.Text                       as T    (Text, concat)
import Data.String                             (fromString)
import Web.Scotty

-- Interpreter
import Hyper.Internal

say = putStrLn

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
defaultPort :: Int
defaultPort = 8024

main :: IO ()
main = do
    env <- System.getEnvironment
    let port = maybe defaultPort id $ readMaybe =<< Prelude.lookup "PORT" env

    (hint, interpreterLoop) <- newInterpreter
    forkIO $ scotty port (jsonAPI hint)
    interpreterLoop -- See NOTE [MainThread]

{- NOTE [MainThread]

We fork the web server and run GHC in the main thread.
This is because GHC registers handlers for signals like SIGTERM,
but for some reason, these handlers only work correctly if GHC
also has the main thread.

-}

{-----------------------------------------------------------------------------
    Exported JSON REST API
------------------------------------------------------------------------------}
jsonAPI :: Hint -> ScottyM ()
jsonAPI hint = do
    post "/cancel" $ do
        liftIO $ cancel hint
        json   $ JSON.object [ "status" .= t "ok" ]
    post "/setExtensions" $
        json . result =<< liftIO . setExtensions hint =<< param "query"
    post "/setImports" $
        json . result =<< liftIO . setImports hint =<< param "query"
    post "/loadFiles" $
        json . result =<< liftIO . loadFiles hint =<< param "query"
    post "/eval" $ do
        json . result =<< liftIO . eval hint =<< param "query"

t s = fromString s :: Text

result :: JSON.ToJSON a => Result a -> JSON.Value
result (Left e) = JSON.object [ "status" .= t "error", "errors" .= err e]
    where
    err e = toJSON $ case e of
        UnknownError s -> [t s]
        WontCompile xs -> map (t . errMsg) xs
        NotAllowed s   -> [t s]
        GhcException s -> [t s]
result (Right x) = JSON.object [ "status" .= t "ok", "value" .= toJSON x ]

instance JSON.ToJSON Graphic where
    toJSON g = JSON.object [ "type" .= t "html", "value" .= gHtml g ]

{-----------------------------------------------------------------------------
    Exported interpreter functions
------------------------------------------------------------------------------}
setImports    :: Hint -> [String]   -> IO (Result ())
setExtensions :: Hint -> [String]   -> IO (Result ())
loadFiles     :: Hint -> [FilePath] -> IO (Result ())
eval          :: Hint -> String     -> IO (Result Graphic)

    -- NOTE: We implicitely load the Prelude and Hyper modules
setImports    hint = run hint . Hint.setImports
                   . (++ ["Prelude", "Hyper"]) . filter (not . null)
setExtensions hint xs = run hint $ Hint.set [Hint.languageExtensions Hint.:= ys]
    where
    readExtension :: String -> Extension
    readExtension x = case readMaybe x of
        Nothing -> error $ "Unknown language extension: " ++ x
        Just x  -> x
    ys = map readExtension $ filter (not . null) xs

loadFiles     hint = run hint . Hint.loadModules . filter (not . null)

-- | Evalute an input cell.
eval         hint input = run hint $ do
    extensions <- Hint.get Hint.languageExtensions
    mgs <- forM (parseStmts extensions input) $ \(code, stmt) -> case stmt of
        Just (Haskell.Qualifier _ _) -> do
            -- NOTE: If it's a simple expression,
            --       we wrap results into an implicit call to Hyper.display
            m <- Hint.interpret ("Hyper.displayIO " ++ Hint.parens code) (as :: IO Graphic)
            liftIO $ do
                g <- m
                x <- evaluate (force g)      -- See NOTE [EvaluateToNF]
                return $ Just x
        _ ->    do
            -- In all other cases, we simply pass the code on to GHC
            Hint.runStmt code
            return Nothing
    return . combineGraphics $ catMaybes mgs

combineGraphics :: [Graphic] -> Graphic
combineGraphics xs = Graphic { gHtml = T.concat $ map gHtml xs }

-- | Statements that we can evaluate.
type Stmt = Haskell.Stmt Haskell.SrcSpanInfo

-- | Parse an input cell into a list of statements to evaluate.
parseStmts :: [Hint.Extension] -> String -> [(String, Maybe Stmt)]
parseStmts extensions =
    map parseStmt . map unlines . groupByIndent . stripIndent . lines
    where
    indent xs      = if null xs then 0 else length . takeWhile (== ' ') $ head xs 
    stripIndent xs = map (drop $ indent xs) xs
    groupByIndent  = groupBy (\x y -> indent [y] > 0)

    parseStmt s    = (s, case Haskell.parseStmtWithMode mode s of
        Haskell.ParseOk x -> Just x
        _                 -> Nothing)

    exts = map (Haskell.parseExtension . show) extensions
    mode = Haskell.defaultParseMode { Haskell.extensions = exts }


{- NOTE [EvaluateToNF]

We evaluate the result in the interpreter thread to full normal form,
because it may be that this evaluation does not terminate,
in which case the user is likely to trigger a `UserInterrupt` asynchronous exception.
But this exception is only delivered to and handled by the interpreter thread.
Otherwise, the web server would be stuck trying to evaluate the result
in order to serialize it, with no way for the user to interrupt this.

-}

{-----------------------------------------------------------------------------
    Interpreter Backend
------------------------------------------------------------------------------}
type Result a = Either InterpreterError a

toInterpreterError :: SomeException -> InterpreterError
toInterpreterError e = case fromException e of
    Just e  -> e
    Nothing -> UnknownError (displayException e)

#if MIN_VERSION_base(4,8,0)
#else
displayException :: SomeException -> String
displayException = show
#endif

-- | Haskell Interpreter.
data Hint = Hint
    { run    :: forall a. Interpreter a -> IO (Result a)
    , cancel :: IO ()
    }

data Action where
    Action :: Interpreter a -> MVar (Result a) -> Action

debug s = liftIO $ putStrLn s

-- | Create and initialize a Haskell interpreter.
newInterpreter :: IO (Hint, IO ()) -- ^ (send commands, interpreter loop)
newInterpreter = do
    vin          <- newEmptyMVar
    evalThreadId <- newEmptyMVar  -- ThreadID of the thread responsible for evaluation
    
    let
        handler :: Interpreter ()
        handler = do
            debug "Waiting for Haskell expression"
            Action ma vout <- liftIO $ takeMVar vin
            let right = liftIO . putMVar vout . Right =<< ma
            let left  = liftIO . putMVar vout . Left . toInterpreterError
            debug "Got Haskell expression, evaluating"
            right `catch` left
            debug "Wrote result"
        
        run :: Interpreter a -> IO (Result a)
        run ma = do
            vout <- newEmptyMVar
            putMVar vin (Action ma vout)
            a <- takeMVar vout
            case a of
                Right _ -> return ()
                Left e  -> debug $ show e
            return a
        
        cancel :: IO ()
        cancel = do
            t <- readMVar evalThreadId
            throwTo t UserInterrupt
            -- NOTE: `throwTo` may block if the thread `t` has masked asynchronous execptions.
            debug "UserInterrupt, evaluation cancelled"
        
        interpreterLoop :: IO ()
        interpreterLoop = do
            putMVar evalThreadId =<< myThreadId
            -- NOTE: The failure branch of `catch` will `mask` asynchronous exceptions.
            let go = forever $ handler `catch` (\UserInterrupt -> return ())
            void $ runInterpreter go

    return (Hint run cancel, interpreterLoop)
