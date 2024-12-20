{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

import Control.Concurrent
    ( forkIO
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
import Data.Aeson
    ( toJSON
    , (.=)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.String
    ( fromString
    )
import Foreign.C.Types
    ( CInt
    )
import GHC.IO.Handle.FD
    ( fdToHandle
    )
import Hyper.Internal
    ( Graphic (..)
    )
import Text.Read
    ( readMaybe
    )

import qualified Data.Aeson as JSON
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified HyperHaskell.Interpreter as H
import qualified System.Environment as System
import qualified System.FilePath.Posix as System
import qualified System.IO as System
import qualified Web.Scotty as Web

-- Interpreter
say :: String -> IO ()
say = putStrLn

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
defaultPort :: Int
defaultPort = 8024

main :: IO ()
main = do
    -- get port number from environment
    env <- System.getEnvironment
    let port = fromMaybe defaultPort $ readMaybe =<< Prelude.lookup "PORT" env

    -- get file descriptor (pipe) from the first argument
    args <- System.getArgs
    let writeReady = case args of
            (x:_) -> maybe (pure ()) writeReadyMsgToFD $ readMaybe x
            _     -> pure ()

    -- Start interpreter and web server. See NOTE [MainThread]
    (hint, interpreterLoop) <- H.newInterpreter writeReady
    forkIO $ Web.scotty port (jsonAPI hint)
    interpreterLoop

-- | Write the message "ready" to the specified file descriptor (pipe).
writeReadyMsgToFD :: CInt -> IO ()
writeReadyMsgToFD fd0 = do
    handle <- GHC.IO.Handle.FD.fdToHandle (fromIntegral fd0)
    System.hSetBinaryMode handle False
    System.hSetEncoding   handle System.utf8
    System.hSetBuffering  handle System.LineBuffering
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
jsonAPI :: H.Hint -> Web.ScottyM ()
jsonAPI hint = do
    Web.post "/cancel" $ do
        liftIO $ H.cancel hint
        Web.json $ JSON.object [ "status" .= t "ok" ]
    Web.post "/setExtensions" $
        Web.json . result =<< liftIO . H.setExtensions hint =<< getParam "query"
    Web.post "/setImports" $
        Web.json . result =<< liftIO . H.setImports hint =<< getParam "query"
    Web.post "/setSearchPath" $ do
        x <- getParam "query"
        y <- getParam "dir"
        Web.json . result =<< liftIO (H.setSearchPath hint x y)
    Web.post "/loadFiles" $
        Web.json . result =<< liftIO . H.loadFiles hint =<< getParam "query"
    Web.post "/eval" $ do
        Web.json . result =<< liftIO . H.eval hint =<< getParam "query"

getParam :: Web.Parsable a => TL.Text -> Web.ActionM a
getParam = Web.formParam

t :: String -> T.Text
t = fromString

-- | Convert an interpreter result to JSON.
result :: JSON.ToJSON a => H.Result a -> JSON.Value
result (Left e) = JSON.object [ "status" .= t "error", "errors" .= err e]
  where
    err e = toJSON $ case e of
        H.UnknownError s -> [t s]
        H.WontCompile xs -> map (t . H.errMsg) xs
        H.NotAllowed s   -> [t s]
        H.GhcException s -> [t s]
result (Right x) = JSON.object [ "status" .= t "ok", "value" .= toJSON x ]

instance JSON.ToJSON Graphic where
    toJSON :: Graphic -> JSON.Value
    toJSON g = JSON.object [ "type" .= t "html", "value" .= gHtml g ]
