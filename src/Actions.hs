module Actions (
    Action
  , actAsAvatar
  , runActions
) where

--------------------------------------------------------------------------------
import           Control.Arrow               (first)
import           Control.Lens                ((.~), (^.), (<.), imapMOf)
import           Control.Monad.Freer         
import           Control.Monad.Freer.State   (get, put, modify, runState, State)
import           Control.Monad.Freer.Writer  (tell, runWriter, Writer)
----------------------------------------------------------------------------------
import           Objects                     (ObjectId(..), Point, Angle, Avatar, 
                                              Object2d, GameWorld, avatars, 
                                              avatarObject, obstacles, obstacleObject,
                                              object2d, objectVelocity, objectRotation,
                                              objectCenter, add, pointZero)
import           ClientActions               (ActionReq(..), ActionResp, moveResp,  
                                              rotateResp)
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------------

-- Game actions 

data Action m where
  AvatarAct       :: ObjectId Avatar -> ActionReq -> Action () 
  Accelerate      :: ObjectId a -> Point -> Action ()
  Rotate          :: ObjectId a -> Angle -> Action ()
  RunSimulation   :: Action ()

-- Constructor for export
actAsAvatar :: Int -> ActionReq -> Action ()
actAsAvatar num = AvatarAct (AvatarId num)
 
-- Sending game actions to freer monad

--avatarAct :: Member Action effs => ObjectId Avatar -> ActionReq -> Eff effs ()
--avatarAct avaId = send . AvatarAct avaId

accelerate :: Member Action effs => ObjectId a -> Point -> Eff effs ()
accelerate objId = send . Accelerate objId 

rotate :: Member Action effs => ObjectId a -> Angle -> Eff effs ()
rotate objId = send . Rotate objId

runSimulation :: Member Action effs => Eff effs ()
runSimulation = send RunSimulation

-- For now just accept everything user throws at you

interpretAvatarAction :: Member Action effs => ObjectId Avatar -> ActionReq -> Eff effs ()
interpretAvatarAction avaId (AccelerateReq p) = accelerate avaId p
interpretAvatarAction avaId (RotateReq r) = rotate avaId r

-- The actual implementation of game

moveByVelocity :: Member (Writer [ActionResp]) effs => ObjectId a -> Object2d -> Eff effs Object2d
moveByVelocity idx object = if vel == pointZero 
  then pure object
  else do
    let npos = add pos vel
    tell [moveResp idx npos]
    return $ objectCenter .~ npos  $ object
  where
  pos = object ^. objectCenter
  vel = object ^. objectVelocity

interpretAction :: Eff '[Action, Writer [ActionResp], State GameWorld] v -> Eff '[Writer [ActionResp], State GameWorld] v
interpretAction = interpret (\case
  AvatarAct avaId p -> interpretAction (interpretAvatarAction avaId p)
  Accelerate objId acc -> modify $ (object2d objId . objectVelocity) .~ acc 
  Rotate objId ang -> do
    modify $ (object2d objId . objectRotation) .~ ang
    tell [rotateResp objId ang]
  RunSimulation -> do
    gw <- get
    ngw <- imapMOf (avatars <. avatarObject) moveByVelocity gw >>= imapMOf (obstacles <. obstacleObject) moveByVelocity
    put ngw)

-- Function to run a sequence of actions in game and recalculate the simulation

runActions :: [Action ()] -> GameWorld -> ([ActionResp], GameWorld)
runActions actions world = first snd
                         $ run . runState world . runWriter 
                         $ traverse interpretAction 
                         $ map send actions ++ [runSimulation]
