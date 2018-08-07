{-# LANGUAGE TemplateHaskell #-}
module Objects where

--------------------------------------------------------------------------------
import           Control.Arrow       (first, (***))
import           Control.Lens        hiding ((.=)) 
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.HashMap.Strict (HashMap) 
import qualified Data.HashMap.Strict as HM
import           Data.Aeson        
import           GHC.Generics
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- ObjectIds
----------------------------------------------------------------------------------

data ObjectId a where
  AvatarId :: Int -> ObjectId Avatar
  ObstacleId :: Int -> ObjectId Obstacle

instance Show (ObjectId a) where
  show (AvatarId i) = "AvatarId " ++ show i
  show (ObstacleId i) = "ObstacleId " ++ show i

data TransportId = TAvatarId (ObjectId Avatar) 
                 | TObstacleId (ObjectId Obstacle)

toTransportId :: ObjectId a -> TransportId
toTransportId aid@(AvatarId _) = TAvatarId aid
toTransportId oid@(ObstacleId _) = TObstacleId oid

instance ToJSON TransportId where
  toJSON (TAvatarId (AvatarId i)) = object
    [ "type" .= toJSON ("AvatarId" :: Text)
    , "num" .= toJSON i
    ]
  toJSON (TObstacleId (ObstacleId i)) = object
    [ "type" .= toJSON ("ObstacleId" :: Text)
    , "num" .= toJSON i
    ]

----------------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------------

newtype Angle = MkAngle { 
  _a :: Double
} deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

data Point = MkPoint {
    _x :: !Double
  , _y :: !Double
} deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

add :: Point -> Point -> Point
add (MkPoint x1 y1) (MkPoint x2 y2) = MkPoint (x1 + x2) (y1 + y2)

data Shape = MkSquare Double
           | MkRectangle Double Double
           | MkCircle Double deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Object for physic implementation

data Object2d = MkObject2d {
    _objectCenter :: Point
  , _objectShape :: Shape
  , _objectRotation :: Angle
  , _objectVelocity :: Point
} deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Avatars for Players (and NPCs?)

data Avatar = MkAvatar {
    _avatarObject :: !Object2d
} deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Noninteractive game objects 

data Obstacle = MkObstacle {
    _obstacleObject :: !Object2d
} deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- Storing it all

type Avatars = HashMap Int Avatar
type Obstacles = HashMap Int Obstacle

data GameWorld = MkGameWorld {
    _gameAvatars :: Avatars
  , _gameObstacles :: Obstacles
} deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

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
object2d a@(AvatarId n) = gameObject a . _Just . avatarObject
object2d o@(ObstacleId n) = gameObject o . _Just . obstacleObject

avatars :: (Indexable (ObjectId Avatar) p, Applicative f) => p Avatar (f Avatar) -> GameWorld -> f GameWorld
avatars = gameAvatars .> reindexed AvatarId itraversed 

obstacles :: (Indexable (ObjectId Obstacle) p, Applicative f) => p Obstacle (f Obstacle) -> GameWorld -> f GameWorld
obstacles = gameObstacles .> reindexed ObstacleId itraversed 

-- Creating some instances

pointZero = MkPoint 0 0

basicObstacles :: Obstacles
basicObstacles = HM.singleton 0 $ MkObstacle $ MkObject2d pointZero (MkSquare 5) (MkAngle 90) pointZero

newAvatarPosition = pointZero
avatarShape = MkCircle 1

startingAvatar :: Avatars
startingAvatar = HM.singleton 0 (MkAvatar $ MkObject2d newAvatarPosition avatarShape (MkAngle 0) pointZero)

gameWorld = MkGameWorld startingAvatar basicObstacles
