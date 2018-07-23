{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Player where

--------------------------------------------------------------------------------
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Control.Lens       (makeLenses)
import qualified Network.WebSockets  as WS
----------------------------------------------------------------------------------


data Player m = MkPlayer {
    _playerGet  :: m Text
  , _playerSend :: Text -> m ()
}

makeLenses ''Player

webscoketPlayer :: WS.Connection -> Player IO
webscoketPlayer conn = MkPlayer (WS.receiveData conn) (WS.sendTextData conn)
