{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.MessagePack.ServerSpec (spec) where

import           Test.Hspec

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async      (race_)
import           Control.Monad.Trans           (liftIO)
import qualified Data.ByteString               as S
import           Network.Socket                (withSocketsDo)

import           Network.MessagePack.Client    (Client)
import qualified Network.MessagePack.Client    as Client
import           Network.MessagePack.Interface (Doc (..), Interface, Returns,
                                                ReturnsM, call, concrete,
                                                interface, method)
import qualified Network.MessagePack.Rpc       as Rpc
import           Network.MessagePack.Server    (MethodDocs (..), MethodVal (..))
import qualified Network.MessagePack.Server    as Server


add :: Int -> Int -> Int
add = (+)

addI :: Interface (Int -> Int -> Returns Int)
addC :: Int -> Int -> Client Int
(addI, addC) = (interface "add" (Arg "a" $ Arg "b" $ Ret "sum"), call . concrete $ addI)

addR :: Rpc.Rpc (Int -> Int -> Returns Int)
addR = Rpc.stubs "add" (Arg "a" $ Arg "b" $ Ret "sum")
  add


echo :: String -> IO String
echo s = return $ "***" ++ s ++ "***"

echoI :: Interface (String -> ReturnsM IO String)
echoC :: String -> Client String
(echoI, echoC) = (interface "echo" (Arg "input" $ RetM "output"), call . concrete $ echoI)


helloR :: Rpc.Rpc (String -> Returns String)
helloR = Rpc.stubs "hello" (Arg "name" $ Ret "hello")
  ("Hello, " ++)


helloIOR :: Rpc.Rpc (Int -> String -> ReturnsM IO String)
helloIOR = Rpc.stubs "helloIO" (Arg "num" $ Arg "name" $ RetM "hello") $
  \num name -> return $ "Hello, " ++ name ++ " " ++ show num


port :: Int
port = 5000


runTest
  :: (S.ByteString -> Int -> Client (Int, String) -> IO (Int, String))
  -> (Int -> [Server.Method IO] -> IO ())
  -> IO ()
runTest runC runS = withSocketsDo $
  server runS `race_` do
    threadDelay 1000
    res <- client runC
    res `shouldBe` (333, "***hello***")


spec :: Spec
spec = do
  describe "simple client" $ do
    it "can communicate with simple server" $
      runTest Client.execClient Server.serve

    it "can communicate with advanced server" $
      runTest Client.execClient Server.runServer

  describe "advanced client" $ do
    it "can communicate with simple server" $
      runTest Client.runClient Server.serve

    it "can communicate with advanced server" $
      runTest Client.runClient Server.runServer

  describe "documentation" $ do
    it "is type-safe" $
      Rpc.docs helloR `shouldBe` ("hello", Arg "name" $ Ret "hello")

    it "works for IO and non-IO" $ do
      Rpc.docs helloR   `shouldBe` ("hello"  , Arg "name" $ Ret "hello")
      Rpc.docs helloIOR `shouldBe` ("helloIO", Arg "num" $ Arg "name" $ RetM "hello")

    it "has working read/show implementations" $ do
      let docs = Rpc.docs addR
      read (show docs) `shouldBe` docs
      show docs `shouldBe` "(\"add\",Arg \"a\" (Arg \"b\" (Ret \"sum\")))"

    it "has type information" $ do
      let meth = Rpc.method helloIOR
      let docs = Server.methodDocs meth
      read (show docs) `shouldBe` docs
      docs `shouldBe` MethodDocs
        { methodArgs =
            [ MethodVal {valName = "num", valType = "Int"}
            , MethodVal {valName = "name", valType = "String"}
            ]
        , methodRetv = MethodVal {valName = "hello", valType = "String"}
        }
      Server.methodName meth `shouldBe` "helloIO"

    it "can be retrieved from type-erased methods" $ do
      let docs = map Server.methodDocs methods
      length docs `shouldNotBe` 0
      mapM_ (\mdoc -> do
          let args = Server.methodArgs mdoc
          let retv = Server.methodRetv mdoc
          length args `shouldNotBe` 0
          mapM_ (\arg -> do
              Server.valName arg `shouldNotBe` ""
              Server.valType arg `shouldNotBe` ""
            ) args
          Server.valName retv `shouldNotBe` ""
          Server.valType retv `shouldNotBe` ""
        ) docs


methods :: [Server.Method IO]
methods =
  [ method addI add
  , method echoI echo
  , Rpc.method helloR
  , Rpc.method helloIOR
  ]


server :: (Int -> [Server.Method IO] -> IO ()) -> IO ()
server run = run port methods


client
  :: (S.ByteString -> Int -> Client (Int, String) -> IO (Int, String))
  -> IO (Int, String)
client run =
  run "127.0.0.1" port $ do
    r1 <- addC 111 222
    liftIO $ r1 `shouldBe` 333
    r2 <- echoC "hello"
    liftIO $ r2 `shouldBe` "***hello***"
    r3 <- Rpc.rpc helloR "iphy"
    liftIO $ r3 `shouldBe` "Hello, iphy"
    r4 <- Rpc.rpc helloIOR 911 "iphy"
    liftIO $ r4 `shouldBe` "Hello, iphy 911"
    return (r1, r2)
