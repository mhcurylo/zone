{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game
    () where

--------------------------------------------------------------------------------
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.IntMap        (IntMap)
import qualified Data.IntMap.Strict as Map
import           Control.Lens       (makeLenses)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
----------------------------------------------------------------------------------


----------------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------------

type Angle = Double

data Point = MkPoint {
    _x :: !Double
  , _y :: !Double
} deriving (Show)

data Shape = MkSquare Double
           | MkRecentlage Double Double
           | MkCircle Double deriving (Show)

data family ObjectId a
data instance ObjectId Avatar = AvatarId Int deriving Show  
data instance ObjectId Obstacle = ObstacleId Int deriving Show 

data Object2d = MkObject2d {
    _objectCenter :: Point
  , _objectShape :: Shape
  , _objectRotation :: Angle
  , _objectVelocity :: Point
} deriving (Show)

data Avatar = MkAvatar {
    _avatarObject :: !Object2d
} deriving (Show)

data Obstacle = MkObstacle {
    _obstacleObject :: !Object2d
} deriving (Show)

type Avatars = IntMap Avatar
type Obstacles = IntMap Obstacle



data GameWorld = MkGameWorld {
    _avatars :: Avatars
  , _obstacles :: Obstacles
} deriving (Show)

makeLenses ''Point
makeLenses ''Object2d
makeLenses ''Avatar
makeLenses ''Obstacle

pointZero = MkPoint 0 0

basicObstacles :: Obstacles
basicObstacles = Map.singleton 0 $ MkObstacle $ MkObject2d pointZero (MkSquare 5) 90 pointZero

newAvatarPosition = pointZero
avatarShape = MkCircle 1

startingAvatar :: ObjectId Avatar -> Avatars
startingAvatar (AvatarId n) = Map.singleton n (MkAvatar $ MkObject2d newAvatarPosition avatarShape 0 pointZero)

gameWorld avatarName = MkGameWorld (startingAvatar avatarName) basicObstacles


----------------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------------

data Action m where
  Move   :: ObjectId a -> Point -> Action ()
  Rotate :: ObjectId a -> Angle -> Action ()

move :: Member Action effs => ObjectId a -> Point -> Eff effs ()
move objId = send . Move objId 

rotate :: Member Action effs => ObjectId a -> Angle -> Eff effs ()
rotate objId = send . Rotate objId

interpretAction :: Action v -> Eff '[State GameWorld] v
interpretAction (Move a p) = pure ()
interpretAction (Rotate a p) = pure ()

sequenceActions :: [Action ()] -> Eff '[State GameWorld] [()]
sequenceActions = mapM interpretAction 

runGameActions :: [Action ()] -> GameWorld -> GameWorld
runGameActions actions gameWorld = snd . run $ (runState gameWorld $ sequenceActions actions)

