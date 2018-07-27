{-# LANGUAGE TemplateHaskell #-}
module Game where

--------------------------------------------------------------------------------
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.IntMap        (IntMap)
import qualified Data.IntMap.Strict as Map
import           Control.Arrow      (first)
import           Control.Lens       
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
----------------------------------------------------------------------------------


----------------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------------

newtype Angle = MkAngle { 
  _a :: Double
} deriving (Show, Num)

data Point = MkPoint {
    _x :: !Double
  , _y :: !Double
} deriving (Show)

data Shape = MkSquare Double
           | MkRecentlage Double Double
           | MkCircle Double deriving (Show)

data ObjectId a where
  AvatarId :: Int -> ObjectId Avatar
  ObstacleId :: Int -> ObjectId Obstacle

-- Object for physic implementation

data Object2d = MkObject2d {
    _objectCenter :: Point
  , _objectShape :: Shape
  , _objectRotation :: Angle
  , _objectVelocity :: Point
} deriving (Show)

-- Avatars for Players (and NPCs?)

data Avatar = MkAvatar {
    _avatarObject :: !Object2d
} deriving (Show)

-- Noninteractive game objects 

data Obstacle = MkObstacle {
    _obstacleObject :: !Object2d
} deriving (Show)

-- Stroing it all

type Avatars = IntMap Avatar
type Obstacles = IntMap Obstacle

data GameWorld = MkGameWorld {
    _gameAvatars :: Avatars
  , _gameObstacles :: Obstacles
} deriving (Show)

makeLenses ''Angle
makeLenses ''Point
makeLenses ''Object2d
makeLenses ''Avatar
makeLenses ''Obstacle

gameObject :: ObjectId a -> Lens' GameWorld (Maybe a)
gameObject (AvatarId n) = lens _gameAvatars (\(MkGameWorld a o) na -> MkGameWorld na o) . at n
gameObject (ObstacleId n) = lens _gameObstacles (\(MkGameWorld a o) no-> MkGameWorld a no) . at n

object2d :: ObjectId a -> Traversal' GameWorld Object2d
object2d a@(AvatarId n) = gameObject a . _Just . avatarObject
object2d o@(ObstacleId n) = gameObject o . _Just . obstacleObject

pointZero = MkPoint 0 0

basicObstacles :: Obstacles
basicObstacles = Map.singleton 0 $ MkObstacle $ MkObject2d pointZero (MkSquare 5) 90 pointZero

newAvatarPosition = pointZero
avatarShape = MkCircle 1

startingAvatar :: Avatars
startingAvatar = Map.singleton 0 (MkAvatar $ MkObject2d newAvatarPosition avatarShape 0 pointZero)

gameWorld = MkGameWorld startingAvatar basicObstacles

----------------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------------

-- Actions accepted from the Player / AI

data ActionReq where
  AccelerateReq   :: Point -> ActionReq
  RotateReq       :: Angle -> ActionReq

-- Responses to the Player 

data ActionResp where
  MoveResp        :: ObjectId a -> Point -> ActionResp
  RotateResp      :: ObjectId a -> Angle -> ActionResp
  GameWorldResp   :: GameWorld -> ActionResp

-- Actual game actions 

data Action m where
  AvatarAct       :: ObjectId Avatar -> ActionReq -> Action () 
  Accelerate      :: ObjectId a -> Point -> Action ()
  Move            :: ObjectId a -> Point -> Action ()
  Rotate          :: ObjectId a -> Angle -> Action ()
  RunSimulation   :: Action ()

-- Sending game actions to freer monad

avatarAct :: Member Action effs => ObjectId Avatar -> ActionReq -> Eff effs ()
avatarAct avaId = send . AvatarAct avaId

accelerate :: Member Action effs => ObjectId a -> Point -> Eff effs ()
accelerate objId = send . Accelerate objId 

move :: Member Action effs => ObjectId a -> Point -> Eff effs ()
move objId = send . Move objId 

rotate :: Member Action effs => ObjectId a -> Angle -> Eff effs ()
rotate objId = send . Rotate objId

runSimulation :: Member Action effs => Eff effs ()
runSimulation = send RunSimulation

-- For now just accept everything user throws at you

interpretAvatarAction :: ObjectId Avatar -> ActionReq -> Eff '[Action, Writer [ActionResp], State GameWorld] ()
interpretAvatarAction avaId (AccelerateReq p) = accelerate avaId p
interpretAvatarAction avaId (RotateReq r) = rotate avaId r

-- The actual implementation of game

interpretAction :: Eff '[Action, Writer [ActionResp], State GameWorld] v -> Eff '[Writer [ActionResp], State GameWorld] v
interpretAction = interpret (\case
  AvatarAct avaId p -> interpretAction (interpretAvatarAction avaId p)
  Accelerate objId acc -> case objId of
      AvatarId id -> return ()
      ObstacleId id -> return ()
  Move objId p -> return ()
  Rotate objId p -> return ()
  RunSimulation -> return ())

-- Function to run a sequence of actions in game and recalculate the simulation

runGameActions :: [Action ()] -> GameWorld -> ([ActionResp], GameWorld)
runGameActions actions world = first snd
                               $ run . runState world . runWriter 
                               $ traverse interpretAction (map send $ actions ++ [RunSimulation])
