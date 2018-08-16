module ClientActions (
    ActionReq(..)
  , ActionResp
  , rotateResp
  , moveResp
  , updateWorld
) where

--------------------------------------------------------------------------------
import           Data.Aeson         ((.=), (.:), object, withObject, FromJSON, 
                                     ToJSON, parseJSON, toJSON, genericParseJSON, 
                                     genericToJSON, Options, defaultOptions, fieldLabelModifier)
import           Data.Char          (toLower)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
----------------------------------------------------------------------------------
import           Objects            (Obstacle, Avatar, ObjectId(..), GameWorld, 
                                     Point, Angle)
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Client Actions
----------------------------------------------------------------------------------

-- TransportId instance for transport

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

instance FromJSON TransportId where
  parseJSON = withObject "ObjectId" $ \o -> do
    type_ <- o .: "type"
    num_ <- o .: "num"
    if type_ == ("AvatarId" :: Text) then
      return $ TAvatarId (AvatarId num_)
    else if type_ == ("ObstacleId" :: Text) then
      return $ TObstacleId (ObstacleId num_)
    else fail ("Bad TransportId type: \"" <> show type_ <> "\"")


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
