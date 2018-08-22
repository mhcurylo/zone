module Zone ( 
   mainLoop
) where

--------------------------------------------------------------------------------
import           Data.Text           (Text)
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
----------------------------------------------------------------------------------
import Game         (newGamePlay, updatePlayerGameWorldIO, runGameLoop)
import PlayerHandle (websocketPlayer)
----------------------------------------------------------------------------------
-- Main server loop
----------------------------------------------------------------------------------

singlePlayerGame :: WS.Connection -> IO ()
singlePlayerGame conn = do
  pHandle   <- websocketPlayer conn
  mvGPlay   <- newGamePlay pHandle
  updatePlayerGameWorldIO mvGPlay
  runGameLoop mvGPlay

app :: WS.ServerApp
app pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  singlePlayerGame conn

readText :: WS.Connection -> IO Text
readText = WS.receiveData

sendText :: WS.Connection -> Text -> IO ()
sendText = WS.sendTextData

mainLoop :: IO ()
mainLoop = WS.runServer "127.0.0.1" 8080 app
