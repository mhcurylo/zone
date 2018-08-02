{-# LANGUAGE TemplateHaskell #-}
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
import           Control.Concurrent         (forkIO)
----------------------------------------------------------------------------------
import           ClientActions
----------------------------------------------------------------------------------

data Monad m => PlayerHandle m = MkPlayer {
    playerGet  :: m [ActionReq]
  , playerSend :: [ActionResp] -> m ()
}

type MVReq = MVar [ActionReq]
type MVResp = MVar [ActionResp]

newMVReq :: IO MVReq
newMVReq = MV.newMVar []

newMVResp :: IO MVResp
newMVResp = MV.newEmptyMVar 

getBatch :: MVReq -> IO [ActionReq]
getBatch = flip MV.modifyMVar $ (\a -> return ([], a))

sendBatch :: MVResp -> [ActionResp] -> IO ()
sendBatch = MV.putMVar 

websocketPlayer :: WS.Connection -> IO (PlayerHandle IO)
websocketPlayer conn = do
  mvReq <- newMVReq
  mvResp <- newMVResp
  forkIO $ forever $ do 
    maybeMsg <- WS.receiveData conn  
    case decode maybeMsg of  
      Nothing -> return ()
      (Just msg) -> MV.modifyMVar_ mvReq (return . (msg :))
  forkIO $ forever $ do 
    cont <- encode <$> MV.takeMVar mvResp
    WS.sendTextData conn cont
  return $ MkPlayer (getBatch mvReq) (sendBatch mvResp)

simplePlayer :: Monad m => [ActionReq] -> PlayerHandle m
simplePlayer arr = MkPlayer (return arr) (\_ -> return ())

getPlayerActions :: Monad m => PlayerHandle m -> m [ActionReq]
getPlayerActions = playerGet

sendPlayerActions :: Monad m => PlayerHandle m -> [ActionResp] -> m ()
sendPlayerActions = playerSend
