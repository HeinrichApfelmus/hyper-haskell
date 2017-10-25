module JavaScript where

import Control.Concurrent

import Data.Text as T
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny

import Hyper

data JSServer = JSServer
    { threadId :: ThreadId
    , window   :: MVar Window
    }

-- | Set up a new JS server
hyper :: IO JSServer 
hyper = do
    w <- newEmptyMVar
    t <- forkIO
        $ startGUI defaultConfig { jsPort = Just 8023, jsWindowReloadOnDisconnect = False }
        $ liftIO . putMVar w
    return $ JSServer { threadId = t, window = w }

connect :: JSServer -> Graphic
connect _ = Hyper.html $ T.pack $
    "Connecting!"
    ++ "<script src='http://localhost:8023/haskell.js'/>"

connect2 :: JSServer -> IO Graphic
connect2 _ = do
    threadDelay $ 1000*1000
    return . Hyper.html $ T.pack $
        "<script type='text/javascript''>Haskell.initFFI('http://localhost:8023/websocket')</script>"

stop :: JSServer -> IO ()
stop = killThread . threadId

displayUI :: JSServer -> UI a -> IO a
displayUI js m = do
    w <- readMVar (window js)
    runUI w $ do
        a <- m
        flushCallBuffer
        return a

alert :: String -> UI ()
alert = runFunction . ffi "alert(%1)"

-- find a way to attach the current evaluation box
-- => The interpreter should offer a genuine JavaScript API?
