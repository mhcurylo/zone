module PlayerHandle (
  PlayerHandle,
  websocketPlayer,
  getPlayerActions,
  sendPlayerActions
  ) where


--------------------------------------------------------------------------------
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Aeson                 (encode, decode)
import           Control.Monad              (forever)
import qualified Network.WebSockets         as WS
import           Control.Concurrent.MVar    (MVar)
import           Control.Concurrent.MVar    as MV 
import           Control.Concurrent.Chan    (Chan)
import           Control.Concurrent.Chan    as CH 
import           Control.Concurrent         (forkIO)
----------------------------------------------------------------------------------
import           ClientActions
----------------------------------------------------------------------------------

data PlayerHandle = MkPlayer {
    playerGet  :: IO [ActionReq]
  , playerSend :: [ActionResp] -> IO ()
}

type MVReq = MVar [ActionReq]
type CHResp = Chan [ActionResp]

newMVReq :: IO MVReq
newMVReq = MV.newMVar []

newCHResp :: IO CHResp
newCHResp = CH.newChan 

getBatch :: MVReq -> IO [ActionReq]
getBatch = flip MV.modifyMVar $ (\a -> return ([], a))

sendBatch :: CHResp -> [ActionResp] -> IO ()
sendBatch = CH.writeChan 

websocketPlayer :: WS.Connection -> IO PlayerHandle
websocketPlayer conn = do
  mvReq <- newMVReq
  chResp <- newCHResp
  forkIO $ forever $ do 
    maybeMsg <- WS.receiveData conn  
    case decode maybeMsg of  
      Nothing -> return ()
      (Just msg) -> MV.modifyMVar_ mvReq (return . (msg :))
  forkIO $ forever $ do 
    cont <- encode <$> CH.readChan chResp
    WS.sendTextData conn cont
  return $ MkPlayer (getBatch mvReq) (sendBatch chResp)

getPlayerActions :: PlayerHandle -> IO [ActionReq]
getPlayerActions = playerGet

sendPlayerActions :: PlayerHandle -> [ActionResp] -> IO ()
sendPlayerActions = playerSend
