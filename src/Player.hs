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
    _playerName :: Text
  , _playerGet  :: m Text
  , _playerSend :: Text -> m ()
}

makeLenses ''Player

webscoketPlayer :: Text -> WS.Connection -> Player IO
webscoketPlayer name conn = MkPlayer name (WS.receiveData conn) (WS.sendTextData conn)
