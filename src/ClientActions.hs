module ClientActions (
    ActionReq(..)
  , ActionResp
  , rotateResp
  , moveResp
  , updateWorld
) where

--------------------------------------------------------------------------------
import           Data.Aeson         (FromJSON, ToJSON, parseJSON, toJSON, genericParseJSON, 
                                     genericToJSON, Options, defaultOptions, fieldLabelModifier)
import           Data.Char          (toLower)
import           GHC.Generics       (Generic)
----------------------------------------------------------------------------------
import           Objects            (ObjectId(..), GameWorld, Point, Angle)
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Client Actions
----------------------------------------------------------------------------------

-- TransportId instance for transport

data TransportId = TAvatarId Int
                 | TObstacleId Int
                deriving (Generic)

toTransportId :: ObjectId a -> TransportId
toTransportId (AvatarId x) = TAvatarId x
toTransportId (ObstacleId x) = TObstacleId x

instance ToJSON TransportId where
  toJSON = genericToJSON defaultOptions

instance FromJSON TransportId where
  parseJSON = genericParseJSON defaultOptions

-- Actions accepted from the Player / AI

actionReqOptions :: Options
actionReqOptions = defaultOptions { fieldLabelModifier = map toLower . drop 5 }

data ActionReq = AccelerateReq { acReqPoint :: Point }
               | RotateReq { roReqAngle :: Angle }
               deriving (Show, Generic)

instance FromJSON ActionReq where
  parseJSON = genericParseJSON actionReqOptions

instance ToJSON ActionReq where
  toJSON = genericToJSON actionReqOptions

-- Responses to the Player 

actionRespOptions :: Options
actionRespOptions = defaultOptions { fieldLabelModifier = map toLower . drop 6 } 

data ActionResp = MoveResp TransportId Point
                | RotateResp TransportId Angle
                | GameWorldResp GameWorld
               deriving (Generic)
               
instance FromJSON ActionResp where
  parseJSON = genericParseJSON actionRespOptions 

instance ToJSON ActionResp where
  toJSON = genericToJSON actionRespOptions

moveResp :: ObjectId a -> Point -> ActionResp
moveResp = MoveResp . toTransportId

rotateResp :: ObjectId a -> Angle -> ActionResp
rotateResp = RotateResp . toTransportId

updateWorld :: GameWorld -> ActionResp
updateWorld = GameWorldResp
