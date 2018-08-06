module Zone
    ( someFunc
        ) where

--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Exception   (finally)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
----------------------------------------------------------------------------------
import Game

----------------------------------------------------------------------------------
-- Main server loop
----------------------------------------------------------------------------------

app :: WS.ServerApp
app pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  T.putStrLn msg
  sendText conn "Hello, get READY 4 GAME"

readText :: WS.Connection -> IO Text
readText = WS.receiveData

sendText :: WS.Connection -> Text -> IO ()
sendText = WS.sendTextData

someFunc :: IO ()
someFunc = WS.runServer "127.0.0.1" 8080 app
