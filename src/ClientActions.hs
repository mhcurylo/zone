module ClientActions where

--------------------------------------------------------------------------------
import           GHC.Generics 
import           Data.Aeson 
----------------------------------------------------------------------------------
import           Objects
----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
-- Client Actions
----------------------------------------------------------------------------------

-- Actions accepted from the Player / AI

data ActionReq = AccelerateReq Point 
               | RotateReq Angle
               deriving (Generic, ToJSON, FromJSON)

-- Responses to the Player 

data ActionResp = MoveResp      TransportId Point 
                | RotateResp    TransportId Angle 
                | GameWorldResp GameWorld 
               deriving (Generic, ToJSON, FromJSON)
