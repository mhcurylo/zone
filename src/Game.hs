{-# LANGUAGE TemplateHaskell #-}
module Game where

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

data GameAction m where
  GetGamePlay       :: GameAction GamePlay
  GetActions        :: PlayerHandle -> GameAction [ActionReq] 
  SendActions       :: PlayerHandle -> [ActionResp] -> GameAction ()
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

sendActions  :: Member GameAction effs => PlayerHandle -> [ActionResp] -> Eff effs ()
sendActions phandle = send . SendActions phandle
 
getTime  :: Member GameAction effs => Eff effs Int
getTime = send GetTime

wait  :: Member GameAction effs => Int -> Eff effs ()
wait = send . Wait

-- Implementation of the Game Loop

getAvatarActions :: Member GameAction effs => PlayerHandles -> Eff effs [Action ()]
getAvatarActions = undefined 

broadcastChanges :: Member GameAction effs => PlayerHandles -> [ActionResp] -> Eff effs ()
broadcastChanges = undefined 

gameTurn :: Member GameAction effs => Eff effs ()
gameTurn = do
  st <- getTime
  (MkGamePlay pHandles gameW) <- getGamePlay
  acts <- getAvatarActions pHandles 
  (resp, newGameW) <- runGame acts gameW 
  broadcastChanges pHandles resp
  saveGamePlay (MkGamePlay pHandles newGameW) 
  ft <- getTime
  wait (st - ft)

-- Interpretation for IO 

systemTimeToMil :: SystemTime -> Int
systemTimeToMil (MkSystemTime s ns) = fromIntegral $ (s `mod` 10000) * 1000000 + (fromIntegral ns) `div` 1000

interpretGameActionIO :: MVar GamePlay -> Eff '[GameAction, IO] a -> IO a  
interpretGameActionIO gamePlayMV = runM . interpretM (\case
  GetGamePlay                 -> MV.takeMVar gamePlayMV 
  (GetActions pHandle)        -> getPlayerActions pHandle
  (SendActions pHandle acts)  -> sendPlayerActions pHandle acts
  (RunGame acts gameW)        -> return $ runActions acts gameW
  (SaveGamePlay gameP)        -> MV.putMVar gamePlayMV gameP
  GetTime                     -> systemTimeToMil <$> getSystemTime 
  (Wait  t)                   -> threadDelay t)

runGameLoop :: MVar GamePlay -> IO ()
runGameLoop = forever . (flip interpretGameActionIO) gameTurn 
