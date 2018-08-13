{-# LANGUAGE TemplateHaskell #-}
module Objects (
    ObjectId(..)
  , Angle, a
  , Point, x, y
  , point
  , add
  , Avatar
  , Obstacle
  , Object2d, objectCenter, objectShape, objectRotation, objectVelocity
  , GameWorld, avatars, obstacles, object2d
  , avatarObject
  , obstacleObject
  , gameObject
  , pointZero
  , gameWorld
) where

----------------------------------------------------------------------------------
import           Control.Lens        hiding ((.=)) 
import           Data.Char           (toLower)
import           Data.HashMap.Strict (HashMap) 
import qualified Data.HashMap.Strict as HM
import           Data.Aeson          (FromJSON, ToJSON, parseJSON, toJSON, genericParseJSON, 
                                      genericToJSON, Options, defaultOptions, fieldLabelModifier)
import           GHC.Generics        (Generic)
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- ObjectIds
----------------------------------------------------------------------------------
data ObjectId a where
  AvatarId :: !Int -> ObjectId Avatar
  ObstacleId :: !Int -> ObjectId Obstacle

instance Show (ObjectId a) where
  show (AvatarId i) = "AvatarId " ++ show i
  show (ObstacleId i) = "ObstacleId " ++ show i

----------------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------------

simpleOptions :: Options
simpleOptions = defaultOptions { fieldLabelModifier = drop 1 }

newtype Angle = MkAngle { 
  _a :: Double
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Angle where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Angle where
  toJSON = genericToJSON simpleOptions

data Point = MkPoint {
    _x :: !Double
  , _y :: !Double
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Point where
  parseJSON = genericParseJSON simpleOptions

instance ToJSON Point where
  toJSON = genericToJSON simpleOptions

point :: Double -> Double -> Point
point = MkPoint

add :: Point -> Point -> Point
add (MkPoint x1 y1) (MkPoint x2 y2) = MkPoint (x1 + x2) (y1 + y2)

data Shape = MkSquare !Double
           | MkRectangle !Double !Double
           | MkCircle !Double deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Object for physic implementation

object2dOptions :: Options
object2dOptions = defaultOptions { fieldLabelModifier = map toLower . drop 7 }

data Object2d = MkObject2d {
    _objectCenter :: Point
  , _objectShape :: Shape
  , _objectRotation :: Angle
  , _objectVelocity :: Point
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Object2d where
  parseJSON = genericParseJSON object2dOptions

instance ToJSON Object2d where
  toJSON = genericToJSON object2dOptions

-- Avatars for Players (and NPCs?)

avatarOptions :: Options
avatarOptions = defaultOptions { fieldLabelModifier = map toLower . drop 7 }

data Avatar = MkAvatar {
    _avatarObject :: !Object2d
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Avatar where
  parseJSON = genericParseJSON avatarOptions

instance ToJSON Avatar where
  toJSON = genericToJSON avatarOptions

-- Noninteractive game objects 

obstacleOptions :: Options
obstacleOptions = defaultOptions { fieldLabelModifier = map toLower . drop 9 }

data Obstacle = MkObstacle {
    _obstacleObject :: !Object2d
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Obstacle where
  parseJSON = genericParseJSON obstacleOptions

instance ToJSON Obstacle where
  toJSON = genericToJSON obstacleOptions


-- Storing it all

type Avatars = HashMap Int Avatar
type Obstacles = HashMap Int Obstacle

gameWorldOptions :: Options
gameWorldOptions = defaultOptions { fieldLabelModifier = map toLower . drop 5 }

data GameWorld = MkGameWorld {
    _gameAvatars :: Avatars
  , _gameObstacles :: Obstacles
} deriving (Show, Eq, Ord, Generic)

instance FromJSON GameWorld where
  parseJSON = genericParseJSON gameWorldOptions

instance ToJSON GameWorld where
  toJSON = genericToJSON gameWorldOptions

makeLenses ''Angle
makeLenses ''Point
makeLenses ''Object2d
makeLenses ''Avatar
makeLenses ''Obstacle
makeLenses ''GameWorld

gameObject :: ObjectId a -> Lens' GameWorld (Maybe a)
gameObject (AvatarId n) = gameAvatars . at n
gameObject (ObstacleId n) = gameObstacles . at n

object2d :: ObjectId a -> Traversal' GameWorld Object2d
object2d aid@(AvatarId _) = gameObject aid . _Just . avatarObject
object2d oid@(ObstacleId _) = gameObject oid . _Just . obstacleObject

avatars :: (Indexable (ObjectId Avatar) p, Applicative f) => p Avatar (f Avatar) -> GameWorld -> f GameWorld
avatars = gameAvatars .> reindexed AvatarId itraversed 

obstacles :: (Indexable (ObjectId Obstacle) p, Applicative f) => p Obstacle (f Obstacle) -> GameWorld -> f GameWorld
obstacles = gameObstacles .> reindexed ObstacleId itraversed 

-- Creating some instances

pointZero :: Point
pointZero = MkPoint 0 0

basicObstacles :: Obstacles
basicObstacles = HM.singleton 0 $ MkObstacle $ MkObject2d pointZero (MkSquare 5) (MkAngle 90) pointZero

newAvatarPosition :: Point
newAvatarPosition = pointZero

avatarShape :: Shape
avatarShape = MkCircle 1

startingAvatar :: Avatars
startingAvatar = HM.singleton 0 (MkAvatar $ MkObject2d newAvatarPosition avatarShape (MkAngle 0) pointZero)

gameWorld :: GameWorld
gameWorld = MkGameWorld startingAvatar basicObstacles
