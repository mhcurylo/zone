module ClientActions where

--------------------------------------------------------------------------------
import           GHC.Generics (Generic)
import           Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON, genericParseJSON, 
                             genericToJSON, Options, defaultOptions, fieldLabelModifier)
import           Data.Char (toLower)
----------------------------------------------------------------------------------
import           Objects (GameWorld, TransportId, Point, Angle)
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Client Actions
----------------------------------------------------------------------------------

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

data ActionResp = MoveResp      { mvRespId :: TransportId, mvRespPoint ::  Point }
                | RotateResp    { roRespId :: TransportId, roRespAngle :: Angle }
                | GameWorldResp { gwRespGameWorld :: GameWorld }
               deriving (Generic)
               
instance FromJSON ActionResp where
  parseJSON = genericParseJSON actionRespOptions 

instance ToJSON ActionResp where
  toJSON = genericToJSON actionRespOptions

updateWorld :: GameWorld -> ActionResp
updateWorld = GameWorldResp
