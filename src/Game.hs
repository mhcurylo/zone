{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
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
----------------------------------------------------------------------------------


----------------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------------

type Angle = Double
newtype AvatarId = MkAvatarId Int deriving Show
newtype ObstacleId = MkObstacleId Int deriving Show

data Point = MkPoint {
    _x :: !Double
  , _y :: !Double
} deriving (Show)

data Shape = MkSquare Double
           | MkRecentlage Double Double
           | MkCircle Double deriving (Show)

data Object2d = MkObject2d {
    _objectCenter :: Point
  , _objectShape :: Shape
  , _objectRotation :: Angle
  , _objectVelocity :: Point
} deriving (Show)

data Avatar = MkAvatar {
    _avatarId :: AvatarId
  , _avatarObject :: !Object2d
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

startingAvatar :: AvatarId -> Avatars
startingAvatar avatarId@(MkAvatarId n) = Map.singleton n (MkAvatar avatarId $ MkObject2d newAvatarPosition avatarShape 0 pointZero)

gameWorld avatarName = MkGameWorld (startingAvatar avatarName) basicObstacles


----------------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------------

data ObjectType = AvatarType | ObstacleType

type family ObjectId a where
  ObjectId AvatarType = AvatarId
  ObjectId ObstacleType = ObstacleId

data Action s m where
  Move   :: ObjectId (a :: ObjectType) -> Point -> Action s ()
  Rotate :: ObjectId (a :: ObjectType) -> Angle -> Action s ()

a :: Action GameWorld ()
a = (Move $ MkAvatarId 0) $ pointZero
