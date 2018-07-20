{-# LANGUAGE OverloadedStrings #-}

module Game
    () where

--------------------------------------------------------------------------------
import           Data.Text           (Text)
import qualified Data.Text          as T
import           Data.IntMap         (IntMap)
import qualified Data.IntMap.Strict as IntMap
----------------------------------------------------------------------------------

newtype XY =  MkXY (Int, Int) deriving Show

newtype Angle = MkAngle Int deriving Show
newtype Move = MkMove Angle deriving Show
newtype Rotate = MkRotate Angle deriving Show

data Player = MkPlayer {
    playerCenter :: !XY
  , playerRotation :: Angle
} deriving (Show)

data Obstacle = MkObstacle {
    obstacleCenter :: !XY
  , obstacleSize :: !XY
  , obstacleRotation :: Angle
} deriving (Show)

type Players = IntMap Player
type Obstacles = [Obstacle]

data GameWorld = MkGameWorld {
    players :: IntMap Player
  , obstacles :: [Obstacle]
} deriving (Show)


basicObstacles :: [Obstacle]
basicObstacles = [MkObstacle (XY (100, 100)) (XY (100, 100)) (Angle 90)]

newPlayerPosition = XY (0,0)
 
startingPlayer :: Players
startingPlayer playerId = IntMap.singleton playerId (MkPlayer newPlayerPosition (Angle 0))

gameWorld = MkGameWorld 
