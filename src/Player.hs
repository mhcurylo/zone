{-# LANGUAGE TemplateHaskell #-}
module Player where

--------------------------------------------------------------------------------
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Maybe 
import           Data.Aeson
import           Control.Monad       (forever, unless)
import           Control.Lens       (makeLenses)
import qualified Network.WebSockets  as WS
import           Control.Concurrent.MVar
import           Control.Concurrent
----------------------------------------------------------------------------------
import           ClientActions
----------------------------------------------------------------------------------

data Player m = MkPlayer {
    playerGet  :: m [ActionReq]
  , playerSend :: [ActionResp] -> m ()
}

type MVReq = MVar [ActionReq]
type MVResp = MVar [ActionResp]

newMVReq :: IO MVReq
newMVReq = newMVar []

newMVResp :: IO MVResp
newMVResp = newEmptyMVar 

getBatch :: MVReq -> IO [ActionReq]
getBatch = flip modifyMVar $ (\a -> return ([], a))

sendBatch :: MVResp -> [ActionResp] -> IO ()
sendBatch = putMVar 


websocketPlayer :: WS.Connection -> IO (Player IO)
websocketPlayer conn = do
  mvReq <- newMVReq
  mvResp <- newMVResp
  forkIO $ forever $ do 
    maybeMsg <- WS.receiveData conn  
    case decode maybeMsg of  
      Nothing -> return ()
      (Just msg) -> modifyMVar_ mvReq (return . (msg :))
  forkIO $ forever $ do 
    cont <- encode <$> takeMVar mvResp
    WS.sendTextData conn cont
  return $ MkPlayer (getBatch mvReq) (sendBatch mvResp)

getActions :: Player m -> m [ActionReq]
getActions = playerGet

sendActions  :: Player m -> m [ActionResp]
sendActions = playerSend
