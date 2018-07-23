{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
    () where

--------------------------------------------------------------------------------
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Map           (Map)
import qualified Data.Map.Strict as Map
import           Control.Lens       (makeLenses)
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

data Object2d = MkObject2d {
    _objectCenter :: Point
  , _objectShape :: Shape
  , _objectRotation :: Angle
  , _objectVelocity :: Point
} deriving (Show)

data Avatar = MkAvatar {
    _avatarName :: Text
  , _avatarObject :: !Object2d
} deriving (Show)

data Obstacle = MkObstacle {
    _obstacleObject :: !Object2d
} deriving (Show)

type Avatars = Map Text Avatar
type Obstacles = [Obstacle]

data GameWorld = MkGameWorld {
    _avatars :: Avatars
  , _obstacles :: Obstacles
} deriving (Show)

makeLenses ''Point
makeLenses ''Object2d
makeLenses ''Avatar
makeLenses ''Obstacle

pointZero = MkPoint 0 0

basicObstacles :: [Obstacle]
basicObstacles = [MkObstacle $ MkObject2d pointZero (MkSquare 5) 90 pointZero]

newAvatarPosition = pointZero
avatarShape = MkCircle 1

startingAvatar :: Text -> Avatars
startingAvatar avatarName = Map.singleton avatarName (MkAvatar avatarName $ MkObject2d newAvatarPosition avatarShape 0 pointZero)

gameWorld avatarName = MkGameWorld (startingAvatar avatarName) basicObstacles


----------------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------------



