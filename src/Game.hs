{-# LANGUAGE TemplateHaskell #-}
module Game (
   GamePlay 
 , newGamePlay
 , runGameLoop  
 , updatePlayerGameWorldIO  
 ) where

--------------------------------------------------------------------------------
import           Control.Lens       
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Control.Monad              (forever)
import           Control.Concurrent.MVar    (MVar)
import           Control.Concurrent.MVar    as MV 
import           Control.Concurrent         (forkIO, threadDelay)
import           Data.HashMap.Strict        (HashMap) 
import qualified Data.HashMap.Strict        as HM
import           Data.Time.Clock.System     (SystemTime(..), getSystemTime)
import           Data.Foldable              (traverse_) 
----------------------------------------------------------------------------------
import           Objects
import           Actions
import           ClientActions
import           PlayerHandle 
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Game actions
----------------------------------------------------------------------------------

type PlayerHandles = HashMap Int PlayerHandle

data GamePlay = MkGamePlay {
     _gamePlayPlayerHandles :: PlayerHandles 
  ,  _gamePlayWorld :: GameWorld  
}

makeLenses ''GamePlay

type MVGamePlay = MVar GamePlay

newGamePlay :: PlayerHandle -> IO MVGamePlay
newGamePlay pHandle = MV.newMVar $ MkGamePlay (HM.singleton 0 pHandle) gameWorld  

data GameAction m where
  GetGamePlay       :: GameAction GamePlay
  GetActions        :: PlayerHandle -> GameAction [ActionReq] 
  SendActions       :: [ActionResp] -> PlayerHandle -> GameAction ()
  RunGame           :: [Action ()] -> GameWorld -> GameAction ([ActionResp], GameWorld)
  SaveGamePlay      :: GamePlay -> GameAction () 
  GetTime           :: GameAction Int
  Wait              :: Int -> GameAction ()

-- Sending game actions to freer monad

getGamePlay :: Member GameAction effs => Eff effs GamePlay
getGamePlay = send GetGamePlay

runGame :: Member GameAction effs => [Action ()] -> GameWorld -> Eff effs ([ActionResp], GameWorld)
runGame acts = send . RunGame acts

saveGamePlay :: Member GameAction effs => GamePlay -> Eff effs () 
saveGamePlay = send . SaveGamePlay

getActions :: Member GameAction effs => PlayerHandle  -> Eff effs [ActionReq] 
getActions = send . GetActions

sendActions  :: Member GameAction effs => [ActionResp] -> PlayerHandle -> Eff effs ()
sendActions acts = send . SendActions acts
 
getTime  :: Member GameAction effs => Eff effs Int
getTime = send GetTime

wait  :: Member GameAction effs => Int -> Eff effs ()
wait = send . Wait

getAvatarActions :: Member GameAction effs => PlayerHandles -> Eff effs [Action ()]
getAvatarActions pHandles = HM.foldl' (++) [] <$> ((flip HM.traverseWithKey $ pHandles) $ \k v -> do
   acts <- getActions v
   return $ map (actAsAvatar k) acts)

broadcastChanges :: Member GameAction effs => [ActionResp] -> PlayerHandles ->Eff effs ()
broadcastChanges acts = traverse_ (sendActions acts)  

waitTillEndOfFrame :: Member GameAction effs => Int -> Int -> Eff effs ()
waitTillEndOfFrame startTime finishTime = wait $ 16000 - (finishTime - startTime)

-- Implementation of the Game Loop

gameTurn :: Member GameAction effs => Eff effs ()
gameTurn = do
  st <- getTime
  (MkGamePlay pHandles gameW) <- getGamePlay
  acts <- getAvatarActions pHandles 
  (resp, newGameW) <- runGame acts gameW 
  broadcastChanges resp pHandles
  saveGamePlay (MkGamePlay pHandles newGameW) 
  ft <- getTime
  waitTillEndOfFrame st ft

updatePlayerGameWorld :: Member GameAction effs => Eff effs ()
updatePlayerGameWorld = do
  gp@(MkGamePlay pHandles gameW) <- getGamePlay
  broadcastChanges [updateWorld gameW] pHandles
  saveGamePlay gp

-- Interpretation for IO 

systemTimeToMil :: SystemTime -> Int
systemTimeToMil (MkSystemTime s ns) = fromIntegral $ (s `mod` 10000) * 1000000 + (fromIntegral ns) `div` 1000

interpretGameActionIO :: MVGamePlay -> Eff '[GameAction, IO] a -> IO a  
interpretGameActionIO gamePlayMV = runM . interpretM (\case
  GetGamePlay                 -> MV.takeMVar gamePlayMV 
  (GetActions pHandle)        -> getPlayerActions pHandle
  (SendActions pHandle acts)  -> sendPlayerActions acts pHandle
  (RunGame acts gameW)        -> return $ runActions acts gameW
  (SaveGamePlay gameP)        -> MV.putMVar gamePlayMV gameP
  GetTime                     -> systemTimeToMil <$> getSystemTime 
  (Wait  t)                   -> threadDelay t)

updatePlayerGameWorldIO :: MVGamePlay -> IO ()
updatePlayerGameWorldIO = (flip interpretGameActionIO) updatePlayerGameWorld

runGameLoop :: MVGamePlay -> IO ()
runGameLoop = forever . (flip interpretGameActionIO) gameTurn 
