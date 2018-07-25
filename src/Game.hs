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
{-# LANGUAGE LambdaCase #-}

module Game
    () where

--------------------------------------------------------------------------------
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.IntMap        (IntMap)
import qualified Data.IntMap.Strict as Map
import           Control.Arrow      (first)
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

-- Actions accepted from the Player / AI

data ActionReq where
  AccelerateReq   :: Point -> ActionReq
  RotateReq       :: Angle -> ActionReq

-- Responses to the Player 

data ActionResp where
  MoveResp        :: ObjectId a -> Point -> ActionResp
  RotateResp      :: ObjectId a -> Angle -> ActionResp

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
  Accelerate objId p -> return ()
  Move objId p -> return ()
  Rotate objId p -> return ()
  RunSimulation -> return ())

-- Function to run a sequence of actions in game and recalculate the simulation

runGameActions :: [Action ()] -> GameWorld -> ([ActionResp], GameWorld)
runGameActions actions world = first snd
                               $ run . runState world . runWriter 
                               $ traverse interpretAction (map send $ actions ++ [RunSimulation])
