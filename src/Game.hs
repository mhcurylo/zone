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
import           Control.Concurrent         (forkIO)
import           Data.HashMap.Strict        (HashMap) 
import qualified Data.HashMap.Strict        as HM
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
  GetActions        :: PlayerHandle -> GameAction [Action ()] 
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

getActions :: Member GameAction effs => PlayerHandle  -> Eff effs [Action ()] 
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

-- interpretGameActionIO :: MVar (GamePlay IO) -> Eff '[GameAction IO, State GamePlay, IO] a -> IO a  
-- interpretGameActionIO gamePlayMV = runM . interpretM (\case
--   Wait              ->  return ())


