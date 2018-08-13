{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
) where


--------------------------------------------------------------------------------
import           Control.Concurrent     (forkIO)
import           Control.Monad          (forever, unless, forM_)
import           Control.Monad.Trans    (liftIO)
import           Network.Socket         (withSocketsDo)
import           Data.Aeson             (encode)
import           Data.Text              (Text)
import           Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Time.Clock.System (SystemTime(..), getSystemTime)
import qualified Network.WebSockets     as WS

----------------------------------------------------------------------------------
import           ClientActions        (ActionReq(..))
import           Objects              (point)
---------------------------------------------------------------------------------

systemTimeToMil :: SystemTime -> Int
systemTimeToMil (MkSystemTime s ns) = fromIntegral (s `mod` 10000) * 1000000 + fromIntegral ns `div` 1000

app :: Text -> WS.ClientApp ()
app appid conn = do
    T.putStrLn $ T.append "Connected! " appid
    WS.sendTextData conn appid 
    WS.sendTextData conn $ encode $ AccelerateReq (point 1.0 1.0)
    forever $ do
        st <- liftIO getSystemTime
        msg <- WS.receiveData conn :: IO Text
        ft <- liftIO getSystemTime
        liftIO $ do 
          T.putStrLn msg
          print $ T.append appid . T.pack . show $ (systemTimeToMil st - systemTimeToMil ft)

main :: IO ()
main = withSocketsDo $ do
  forM_ ["a", "b", "c", "d", "e"] (\appid -> 
    forkIO $ WS.runClient "127.0.0.1" 8080 "/" $ app appid) 
  WS.runClient "127.0.0.1" 8080 "/" $ app "z"
