{-# LANGUAGE TemplateHaskell #-}
module Objects where

--------------------------------------------------------------------------------
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.IntMap        (IntMap)
import qualified Data.IntMap.Strict as Map
import           Control.Arrow      (first, (***))
import           Control.Lens      hiding ((.=)) 
import qualified Data.HashMap.Strict as HM
import           Data.Aeson        
import           GHC.Generics
----------------------------------------------------------------------------------

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

data ObjectId a where
  AvatarId :: Int -> ObjectId Avatar
  ObstacleId :: Int -> ObjectId Obstacle

instance ToJSON (ObjectId a) where
  toJSON :: ObjectId a -> Value
  toJSON (AvatarId i) = object
    [ "id" .= toJSON ("AvatarId" :: Text)
    , "num" .= toJSON i
    ]
  toJSON (ObstacleId i) = object
    [ "id" .= toJSON ("ObstacleId" :: Text)
    , "num" .= toJSON i
    ]

instance FromJSON (ObjectId Avatar) where
  parseJSON = withObject "ObjectId" $ \o -> do
    id_ <- o .: "id"
    num_ <- o .: "num"
    if id_ /= ("AvatarId" :: Text)
      then fail "Not an AvatarId!"
      else return $ AvatarId num_

instance FromJSON (ObjectId Obstacle) where
  parseJSON = withObject "ObjectId" $ \o -> do
    id_ <- o .: "id"
    num_ <- o .: "num"
    if id_ /= ("ObstacleId" :: Text)
      then fail "Not an ObstacleId!"
      else return $ ObstacleId num_

instance Show (ObjectId a) where
  show (AvatarId i) = "AvatarId " ++ show i
  show (ObstacleId i) = "ObstacleId " ++ show i

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

type Avatars = IntMap Avatar
type Obstacles = IntMap Obstacle

data GameWorld = MkGameWorld {
    _gameAvatars :: Avatars
  , _gameObstacles :: Obstacles
} deriving (Show, Eq, Ord)

--instance FromJSON GameWorld where
--  parseJSON o = do
--    avatars <- obj .: "avatars"
--    obstacles <- obj .: "obstacles"
--    avatars' <- Map.fromList $ map ((read :: (Text -> Int)) *** decode) $ HM.toList avatars
--    obstacles' <- Map.fromList $ map ((read :: (Text -> Int)) *** decode) $ HM.toList obstacles
--    return $ MkGameWorld avatars obstacles

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
basicObstacles = Map.singleton 0 $ MkObstacle $ MkObject2d pointZero (MkSquare 5) (MkAngle 90) pointZero

newAvatarPosition = pointZero
avatarShape = MkCircle 1

startingAvatar :: Avatars
startingAvatar = Map.singleton 0 (MkAvatar $ MkObject2d newAvatarPosition avatarShape (MkAngle 0) pointZero)

gameWorld = MkGameWorld startingAvatar basicObstacles
