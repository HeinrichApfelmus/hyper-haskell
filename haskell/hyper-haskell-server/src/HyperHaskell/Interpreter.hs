{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module HyperHaskell.Interpreter
    ( -- * Interpreter session
      Hint
    , Result (..)
    , Hint.InterpreterError (..)
    , Hint.errMsg
    , newInterpreter

    -- * Interpreter actions
    , cancel
    , setExtensions
    , setImports
    , setSearchPath
    , loadFiles
    , eval
    ) where

import Control.Concurrent
    ( MVar
    , forkIO
    , myThreadId
    , throwTo
    , newEmptyMVar
    , putMVar
    , readMVar
    , takeMVar
    )
import Control.DeepSeq
    ( force
    )
import Control.Exception
    ( AsyncException (UserInterrupt)
    , evaluate
    )
import Control.Monad
    ( forever
    , forM
    , void
    )
import Control.Monad.Catch
    ( SomeException
    , catch
    , displayException
    , fromException
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.List
    ( groupBy
    )
import Data.Maybe
    ( fromMaybe
    , catMaybes
    )
import Hyper.Internal
    ( Graphic (..)
    )
import Text.Read
    ( readMaybe
    )

import qualified Data.Text as T
import qualified Language.Haskell.Interpreter as Hint
import qualified Language.Haskell.Exts as Haskell
import qualified Hyper.Internal as Hyper
import qualified System.FilePath.Posix as System

{-----------------------------------------------------------------------------
    Interpreter
    Exports
------------------------------------------------------------------------------}
-- | Set Haskell language extensions used in the current interpreter session.
setExtensions :: Hint -> [String]   -> IO (Result ())
setExtensions hint xs =
    run hint $ Hint.set [Hint.languageExtensions Hint.:= ys]
  where
    readExtension :: String -> Hint.Extension
    readExtension x = case readMaybe x of
        Nothing -> error $ "Unknown language extension: " ++ x
        Just x  -> x
    ys = map readExtension $ filter (not . null) xs

-- | Set module imports used in the current interpreter session.
--
-- NOTE: We implicitly always load the "Prelude" and "Hyper" modules.
setImports :: Hint -> [String] -> IO (Result ())
setImports hint =
    run hint . Hint.setImportsF
    . (++ map simpleImport ["Prelude", "Hyper"])
    . map (parseImport . words)
    . filter (not . null)

mkModuleImport :: String -> Hint.ModuleQualification -> Hint.ModuleImport
mkModuleImport m q = Hint.ModuleImport m q Hint.NoImportList

simpleImport :: String -> Hint.ModuleImport
simpleImport m = mkModuleImport m Hint.NotQualified

parseImport :: [String] -> Hint.ModuleImport
parseImport (x:xs) =
    if x == "import" then parse xs else parse (x:xs)
  where
    parse ["qualified", m, "as", alias] =
        mkModuleImport m (Hint.QualifiedAs $ Just alias)
    parse ["qualified", m] = mkModuleImport m (Hint.QualifiedAs Nothing)
    parse [m, "as", alias] = mkModuleImport m (Hint.ImportAs alias)
    parse [m] = mkModuleImport m Hint.NotQualified

-- | Set search path for loading `.hs` source files
setSearchPath :: Hint -> String -> FilePath -> IO (Result ())
setSearchPath hint xs dir =
    run hint $ Hint.set [Hint.searchPath Hint.:= paths]
  where
    paths =
        [ if System.isRelative x then dir System.</> x else x
        | x <- System.splitSearchPath xs
        ]

-- | Load `.hs` source files.
loadFiles :: Hint -> [FilePath] -> IO (Result ())
loadFiles hint xs = run hint $ do
    -- liftIO . print =<< Hint.get Hint.searchPath
    finalizeSession    -- finalize the old session
    Hint.loadModules $ filter (not . null) xs

-- | Evaluate an input cell.
eval :: Hint -> String -> IO (Result Graphic)
eval hint input = run hint $ do
    extensions <- Hint.get Hint.languageExtensions
    mgs <- forM (parsePrompts extensions input) $ \prompt -> case prompt of
        Expr   code -> do
            -- To show the result of an expression,
            -- we wrap results into an implicit call to Hyper.display
            m <- Hint.interpret ("Hyper.displayIO " ++ Hint.parens code)
                    (Hint.as :: IO Graphic)
            liftIO $ do
                g <- m
                x <- evaluate (force g)      -- See NOTE [EvaluateToNF]
                pure $ Just x
        Other  code -> do
            -- Otherwise, there is nothing to show and we pass the code on to GHC
            Hint.runStmt code
            pure Nothing
        TypeOf code -> do
            -- Query type information
            let pre s = "<pre>" ++ code ++ " :: "++ s ++ "</pre>"
            Just . Hyper.html . T.pack . pre <$> Hint.typeOf code
        Unknown code -> do
            pure . Just . Hyper.string
            $ "Unknown interpreter command :" ++ code
    pure . combineGraphics $ catMaybes mgs

combineGraphics :: [Graphic] -> Graphic
combineGraphics xs = Graphic { gHtml = T.concat $ map gHtml xs }

-- | Statements that we can evaluate.
type Stmt = Haskell.Stmt Haskell.SrcSpanInfo

data Prompt
    = Expr String
    | TypeOf String
    | Other String
    | Unknown String

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
    Interpreter
    Internals
------------------------------------------------------------------------------}
-- | Finalize a session.
finalizeSession :: Hint.Interpreter ()
finalizeSession = do
    setImportsInternal
    Hint.runStmt ("Hyper.Internal.finalizeSession")

-- | Clear imports and import only the "Hyper.Internal" module qualified.
setImportsInternal :: Hint.Interpreter ()
setImportsInternal = do
    let name = "Hyper.Internal"
    Hint.setImportsF
        [Hint.ModuleImport name (Hint.QualifiedAs $ Just name) Hint.NoImportList]

{-
-- | Run an interpreter action with only the "Hyper.Internal" module loaded.
withInternal :: Interpreter a -> Interpreter a
withInternal m = do
    xs <- Hint.getLoadedModules
    let name = "Hyper.Internal"
    Hint.setImportsQ [Hint.ModuleImport name (QualifiedAs $ Just name) NoImportList]
    a  <- m
    Hint.setImportsQ xs
    pure a
-}

{-----------------------------------------------------------------------------
    Interpreter
    Thread
------------------------------------------------------------------------------}
type Result a = Either Hint.InterpreterError a

toInterpreterError :: SomeException -> Hint.InterpreterError
toInterpreterError e = case fromException e of
    Just e  -> e
    Nothing -> Hint.UnknownError (displayException e)

#if MIN_VERSION_base(4,8,0)
#else
displayException :: SomeException -> String
displayException = show
#endif

-- | Haskell Interpreter.
data Hint = Hint
    { run    :: forall a. Hint.Interpreter a -> IO (Result a)
    , cancel :: IO ()
    }

data Action where
    Action :: Hint.Interpreter a -> MVar (Result a) -> Action

debug :: Hint.MonadIO m => String -> m ()
debug s = liftIO $ putStrLn s

-- | Create and initialize a Haskell interpreter.
-- Arguments:
--   writeReady = function to call when the interpreter is ready to evaluate expressions.
newInterpreter :: IO () -> IO (Hint, IO ()) -- ^ (send commands, interpreter loop)
newInterpreter writeReady = do
    vin          <- newEmptyMVar
    evalThreadId <- newEmptyMVar  -- ThreadID of the thread responsible for evaluation

    let
        handler :: Hint.Interpreter ()
        handler = do
            debug "Waiting for Haskell expression"
            Action ma vout <- liftIO $ takeMVar vin
            let right = liftIO . putMVar vout . Right =<< ma
            let left  = liftIO . putMVar vout . Left . toInterpreterError
            debug "Got Haskell expression, evaluating"
            right `catch` left
            debug "Wrote result"

        run :: Hint.Interpreter a -> IO (Result a)
        run ma = do
            vout <- newEmptyMVar
            putMVar vin (Action ma vout)
            a <- takeMVar vout
            case a of
                Right _ -> pure ()
                Left e  -> debug $ show e
            pure a

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
            let go = forever $ handler `catch` (\UserInterrupt -> pure ())
            void $ Hint.runInterpreter $ liftIO writeReady >> go

    pure (Hint run cancel, interpreterLoop)
