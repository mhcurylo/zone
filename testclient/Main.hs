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
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Time.Clock.System (SystemTime(..), getSystemTime)
import qualified Network.WebSockets     as WS

----------------------------------------------------------------------------------
import           ClientActions
import           Objects
---------------------------------------------------------------------------------

systemTimeToMil :: SystemTime -> Int
systemTimeToMil (MkSystemTime s ns) = fromIntegral $ (s `mod` 10000) * 1000000 + (fromIntegral ns) `div` 1000

app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"
    WS.sendTextData conn ("Boowie!" :: Text)
    WS.sendTextData conn $ encode $ AccelerateReq (MkPoint 1.0 1.0)
    forever $ do
        st <- liftIO $ getSystemTime
        msg <- (WS.receiveData conn :: IO Text)
        ft <- liftIO $ getSystemTime
        return ()
        liftIO $ do 
          T.putStrLn msg
          putStrLn $ show (systemTimeToMil st - systemTimeToMil ft)

main :: IO ()
main = withSocketsDo $ do
    WS.runClient "127.0.0.1" 8080 "/" app
