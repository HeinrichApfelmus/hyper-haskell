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

-- System environment and Inter-Process-Communication (IPC)
import Foreign.C.Types ( CInt )
import GHC.IO.Handle.FD ( fdToHandle )
import System.Environment    as System
import System.FilePath.Posix as System
import System.IO             as System

-- Haskell interpreter
import           Language.Haskell.Interpreter hiding (eval, setImports)
import qualified Language.Haskell.Interpreter as Hint

-- Haskell language parsing
import qualified Language.Haskell.Exts as Haskell

-- web
import           Data.Aeson                    (toJSON, (.=))
import qualified Data.Aeson            as JSON
import qualified Data.ByteString.Char8 as B
import Data.Text                       as T    (Text, concat, pack)
import Data.String                             (fromString)
import Web.Scotty

-- Interpreter
import Hyper.Internal                  as Hyper

say = putStrLn

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
defaultPort :: Int
defaultPort = 8024

main :: IO ()
main = do
    -- get port number from environment
    env  <- System.getEnvironment
    let port = maybe defaultPort id $ readMaybe =<< Prelude.lookup "PORT" env

    -- get file descriptor (pipe) from the first argument
    args <- System.getArgs
    let writeReady = case args of
            (x:_) -> maybe (return ()) writeReadyMsgToFD $ readMaybe x
            _     -> return ()

    -- Start interpreter and web server. See NOTE [MainThread]
    (hint, interpreterLoop) <- newInterpreter writeReady
    forkIO $ scotty port (jsonAPI hint)
    interpreterLoop

-- | Write the message "ready" to the specified file descriptor (pipe).
writeReadyMsgToFD :: CInt -> IO ()
writeReadyMsgToFD fd0 = do
    handle <- GHC.IO.Handle.FD.fdToHandle (fromIntegral fd0)
    System.hSetBinaryMode handle False
    System.hSetEncoding   handle System.utf8
    System.hSetBuffering  handle LineBuffering
    System.hPutStr        handle "ready"


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
    post "/setSearchPath" $ do
        x <- param "query"
        y <- param "dir"
        json . result =<< (liftIO $ setSearchPath hint x y)
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
setExtensions :: Hint -> [String]   -> IO (Result ())
setImports    :: Hint -> [String]   -> IO (Result ())
setSearchPath :: Hint -> String -> FilePath -> IO (Result ())
loadFiles     :: Hint -> [FilePath] -> IO (Result ())
eval          :: Hint -> String     -> IO (Result Graphic)

-- | Set Haskell language extensions used in the current interpreter session.
setExtensions hint xs = run hint $ Hint.set [Hint.languageExtensions Hint.:= ys]
    where
    readExtension :: String -> Extension
    readExtension x = case readMaybe x of
        Nothing -> error $ "Unknown language extension: " ++ x
        Just x  -> x
    ys = map readExtension $ filter (not . null) xs


-- | Set module imports used in the current interpreter session.
-- NOTE: We implicitly always load the Prelude and Hyper modules
setImports    hint = run hint . Hint.setImportsF
                   . (++ map simpleImport ["Prelude", "Hyper"])
                   . map (parseImport . words)
                   . filter (not . null)

moduleImport m q = Hint.ModuleImport m q NoImportList

simpleImport :: String -> Hint.ModuleImport
simpleImport m = moduleImport m NotQualified

parseImport :: [String] -> Hint.ModuleImport
parseImport (x:xs) = if x == "import" then parse xs else parse (x:xs)
    where
    parse ("qualified":m:"as":alias:[]) = moduleImport m (QualifiedAs $ Just alias)
    parse ("qualified":m:[]) = moduleImport m (QualifiedAs Nothing)
    parse (m:"as":alias:[])  = moduleImport m (ImportAs alias)
    parse (m:[])             = moduleImport m NotQualified


-- | Set search path for loading `.hs` source files
setSearchPath hint xs dir = run hint $ Hint.set [Hint.searchPath Hint.:= paths]
    where paths = [if isRelative x then dir </> x else x | x <- splitSearchPath xs]

-- | Load `.hs` source files.
loadFiles     hint xs = run hint $ do
    -- liftIO . print =<< Hint.get Hint.searchPath
    Main.finalizeSession    -- finalize the old session
    Hint.loadModules $ filter (not . null) xs

-- | Evaluate an input cell.
eval         hint input = run hint $ do
    extensions <- Hint.get Hint.languageExtensions
    mgs <- forM (parsePrompts extensions input) $ \prompt -> case prompt of
        Expr   code -> do
            -- To show the result of an expression,
            -- we wrap results into an implicit call to Hyper.display
            m <- Hint.interpret ("Hyper.displayIO " ++ Hint.parens code)
                    (as :: IO Graphic)
            liftIO $ do
                g <- m
                x <- evaluate (force g)      -- See NOTE [EvaluateToNF]
                return $ Just x
        Other  code -> do
            -- Otherwise, there is nothing to show and we pass the code on to GHC
            Hint.runStmt code
            return Nothing
        TypeOf code -> do
            -- Query type information
            let pre s = "<pre>" ++ code ++ " :: "++ s ++ "</pre>"
            Just . Hyper.html . T.pack . pre <$> Hint.typeOf code
        Unknown code -> do
            return . Just . string $ "Unknown interpreter command :" ++ code
    return . combineGraphics $ catMaybes mgs

combineGraphics :: [Graphic] -> Graphic
combineGraphics xs = Graphic { gHtml = T.concat $ map gHtml xs }

-- | Statements that we can evaluate.
type Stmt   = Haskell.Stmt Haskell.SrcSpanInfo

data Prompt = Expr String | TypeOf String | Other String | Unknown String

-- | Parse an input cell into a list of prompts to evaluate
parsePrompts :: [Hint.Extension] -> String -> [Prompt]
parsePrompts extensions =
    map parsePrompt . map unlines . groupByIndent . stripIndent . lines
    where
    indent xs      = if null xs then 0 else length . takeWhile (== ' ') $ head xs 
    stripIndent xs = map (drop $ indent xs) xs
    groupByIndent  = groupBy (\x y -> indent [y] > 0)

    parsePrompt code@(':':command) = case words command of
        ("type":xs) -> TypeOf $ unwords xs
        (x:_)      -> Unknown x
        []         -> Unknown "(empty)"
    parsePrompt code =
        case Haskell.parseStmtWithMode mode code :: Haskell.ParseResult Stmt of
            Haskell.ParseOk (Haskell.Qualifier _ _) -> Expr code
            _  -> Other code

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
    Internal interpreter functions
------------------------------------------------------------------------------}
-- | 
finalizeSession :: Interpreter ()
finalizeSession = do
    Main.setImportsInternal
    Hint.runStmt ("Hyper.Internal.finalizeSession")

-- | Clear imports and import only the "Hyper.Internal" module qualified.
setImportsInternal :: Interpreter ()
setImportsInternal = do
    let name = "Hyper.Internal"
    Hint.setImportsF [Hint.ModuleImport name (QualifiedAs $ Just name) NoImportList]

{-
-- | Run an interpreter action with only the "Hyper.Internal" module loaded.
withInternal :: Interpreter a -> Interpreter a
withInternal m = do
    xs <- Hint.getLoadedModules
    let name = "Hyper.Internal"
    Hint.setImportsQ [Hint.ModuleImport name (QualifiedAs $ Just name) NoImportList]
    a  <- m
    Hint.setImportsQ xs
    return a
-}

{-----------------------------------------------------------------------------
    Interpreter Thread
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
-- Arguments:
--   writeReady = function to call when the interpreter is ready to evaluate expressions.
newInterpreter :: IO () -> IO (Hint, IO ()) -- ^ (send commands, interpreter loop)
newInterpreter writeReady = do
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
            void $ runInterpreter $ liftIO writeReady >> go

    return (Hint run cancel, interpreterLoop)
