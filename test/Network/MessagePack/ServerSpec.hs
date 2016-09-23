{-# LANGUAGE OverloadedStrings #-}
module Network.MessagePack.ServerSpec (spec) where

import           Test.Hspec

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.Async   (race_)
import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString            as S
import           Network                    (withSocketsDo)

import           Network.MessagePack.Client
import           Network.MessagePack.Server


add :: Int -> Int -> Int
add = (+)

addC :: Int -> Int -> Client Int
addC = call "add"

addM :: Int -> Int -> Server Int
addM x y = return $ add x y


echo :: String -> String
echo s = "***" ++ s ++ "***"

echoC :: String -> Client String
echoC = call "echo"

echoM :: String -> Server String
echoM = return . echo


port :: Int
port = 5000


runTest
  :: (S.ByteString -> Int -> Client (Int, String) -> IO (Int, String))
  -> (Int -> [Method IO] -> IO ())
  -> IO ()
runTest runC runS = withSocketsDo $
  server runS `race_` do
    threadDelay 1000
    res <- client runC
    res `shouldBe` (123 + 456, "***hello***")


spec :: Spec
spec = do
  describe "simple client" $ do
    it "can communicate with simple server" $
      runTest execClient serve

    it "can communicate with advanced server" $
      runTest execClient runServer

  describe "advanced client" $ do
    it "can communicate with simple server" $
      runTest runClient serve

    it "can communicate with advanced server" $
      runTest runClient runServer


server :: (Int -> [Method IO] -> IO ()) -> IO ()
server run =
  run port
    [ method "add" addM
    , method "echo" echoM
    ]


client
  :: (S.ByteString -> Int -> Client (Int, String) -> IO (Int, String))
  -> IO (Int, String)
client run =
  run "127.0.0.1" port $ do
    r1 <- addC 123 456
    liftIO $ r1 `shouldBe` 123 + 456
    r2 <- echoC "hello"
    liftIO $ r2 `shouldBe` "***hello***"
    return (r1, r2)
