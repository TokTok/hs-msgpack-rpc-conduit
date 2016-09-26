{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.MessagePack.ServerSpec (spec) where

import           Test.Hspec

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async      (race_)
import           Control.Monad.Trans           (liftIO)
import qualified Data.ByteString               as S
import           Network                       (withSocketsDo)

import           Network.MessagePack.Client    (Client, execClient, runClient)
import           Network.MessagePack.Interface
import qualified Network.MessagePack.Rpc       as Rpc
import           Network.MessagePack.Server    (Method, runServer, serve)


add :: Int -> Int -> Int
add = (+)

addI :: Interface (Int -> Int -> Returns Int)
addC :: Int -> Int -> Client Int
(addI, addC) = (interface "add", call . concrete $ addI)


echo :: String -> IO String
echo s = return $ "***" ++ s ++ "***"

echoI :: Interface (String -> Returns String)
echoC :: String -> Client String
(echoI, echoC) = (interface "echo", call . concrete $ echoI)


helloR :: Rpc.Rpc IO (String -> Returns String)
helloR = Rpc.stubs "hello" ("Hello, " ++)


helloIOR :: Rpc.RpcIO IO (String -> Returns String)
helloIOR = Rpc.stubsIO "helloIO" (return . ("Hello, " ++))


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


methods :: [Method IO]
methods =
  [ method addI add
  , methodIO echoI echo
  , Rpc.method helloR
  , Rpc.method helloIOR
  ]


server :: (Int -> [Method IO] -> IO ()) -> IO ()
server run = run port methods


client
  :: (S.ByteString -> Int -> Client (Int, String) -> IO (Int, String))
  -> IO (Int, String)
client run =
  run "127.0.0.1" port $ do
    r1 <- addC 123 456
    liftIO $ r1 `shouldBe` 123 + 456
    r2 <- echoC "hello"
    liftIO $ r2 `shouldBe` "***hello***"
    r3 <- Rpc.rpc helloR "iphy"
    liftIO $ r3 `shouldBe` "Hello, iphy"
    r4 <- Rpc.rpc helloIOR "iphy"
    liftIO $ r4 `shouldBe` "Hello, iphy"
    return (r1, r2)
