{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Trustworthy                #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Server
-- Copyright : (c) Hideyuki Tanaka, 2010-2015
-- License   : BSD3
--
-- Maintainer:  tanaka.hideyuki@gmail.com
-- Stability :  experimental
-- Portability: portable
--
-- This module is server library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at
-- <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- > import Network.MessagePackRpc.Server
-- >
-- > add :: Int -> Int -> Server Int
-- > add x y = return $ x + y
-- >
-- > main = serve 1234 [ method "add" add ]
--
--------------------------------------------------------------------

module Network.MessagePack.Server.Basic (
  -- * RPC method types
    Method
  , MethodVal (..)
  , MethodDocs (..)
  , MethodType (..)
  , ServerT (..)
  , Server

  -- * Build a method
  , method

  -- * Get the method name
  , methodName
  , methodDocs

  -- * Start RPC server
  , serve
  ) where

import           Control.Applicative               (Applicative, pure, (<$>),
                                                    (<|>))
import           Control.Monad.Catch               (MonadCatch, MonadThrow,
                                                    catch, throwM)
import           Control.Monad.Trans               (MonadIO, MonadTrans, lift)
import           Control.Monad.Trans.Control       (MonadBaseControl)
import qualified Data.Binary                       as Binary
import qualified Data.ByteString                   as S
import           Data.Conduit                      (ResumableSource, Sink, ($$),
                                                    ($$+), ($$++))
import qualified Data.Conduit.Binary               as CB
import           Data.Conduit.Network              (appSink, appSource,
                                                    runGeneralTCPServer,
                                                    serverSettings,
                                                    setAfterBind)
import           Data.Conduit.Serialization.Binary (ParseError, sinkGet)
import qualified Data.List                         as List
import           Data.MessagePack                  (MessagePack, Object,
                                                    fromObject, toObject)
import qualified Data.MessagePack.Result           as R
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Traversable                  (sequenceA)
import           Network.Socket                    (SocketOption (ReuseAddr),
                                                    setSocketOption)

import           Network.MessagePack.Types

data MethodVal = MethodVal
  { valName :: !Text
  , valType :: !Text
  }
  deriving (Show)

data MethodDocs = MethodDocs
  { methodArgs :: ![MethodVal]
  , methodRetv :: !MethodVal
  }
  deriving (Show)

-- ^ MessagePack RPC method
data Method m = Method
  { methodName :: !Text
  , methodDocs :: !MethodDocs
  , methodBody :: [Object] -> m Object
  }


newtype ServerT m a = ServerT { runServerT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)


instance MonadTrans ServerT where
  lift = ServerT


type Server = ServerT IO


class Monad m => MethodType m f where
  -- | Create a RPC method from a Haskell function
  toBody :: Text -> f -> [Object] -> m Object


instance (Functor m, MonadThrow m, MessagePack o) => MethodType m (ServerT m o) where
  toBody _ m [] = toObject <$> runServerT m
  toBody n _ ls =
    throwM $ ServerError $
      "invalid arguments for method '" <> n <> "': " <> T.pack (show ls)


instance (MonadThrow m, MessagePack o, MethodType m r) => MethodType m (o -> r) where
  toBody n f (x : xs) =
    case fromObject x of
      Nothing -> throwM $ ServerError "argument type error"
      Just r  -> toBody n (f r) xs
  toBody _ _ [] = error "messagepack-rpc methodtype instance toBody failed"


-- | Build a method
method
  :: MethodType m f
  => Text     -- ^ Method name
  -> MethodDocs
  -> f        -- ^ Method body
  -> Method m
method name docs body = Method name docs $ toBody name body


processRequests
  :: (Applicative m, MonadThrow m, MonadCatch m)
  => [Method m]
  -> ResumableSource m S.ByteString
  -> Sink S.ByteString m t
  -> m b
processRequests methods rsrc sink = do
  (rsrc', res) <-
    rsrc $$++ do
      obj <- sinkGet Binary.get
      case unpackRequest obj of
        Nothing ->
          throwM $ ServerError "invalid request"
        Just req@(_, msgid, _, _) ->
          lift $ getResponse methods req `catch` \(ServerError err) ->
            return (1, msgid, toObject err, toObject ())

  _ <- CB.sourceLbs (packResponse res) $$ sink
  processRequests methods rsrc' sink


getResponse
  :: Applicative m
  => [Method m]
  -> Request Object
  -> m Response
getResponse methods (0, msgid, mth, args) =
  process <$> callMethod methods mth args
  where
    process (R.Failure err) = (1, msgid, toObject err, toObject ())
    process (R.Success ok ) = (1, msgid, toObject (), ok)

getResponse _ (rtype, msgid, _, _) =
  pure (1, msgid, toObject ["request type is not 0, got " <> T.pack (show rtype)], toObject ())


callMethod
  :: (Applicative m)
  => [Method m]
  -> Object
  -> [Object]
  -> m (R.Result Object)
callMethod methods mth args = sequenceA $
  (stringCall =<< fromObject mth)
  <|>
  (intCall =<< fromObject mth)

  where
    stringCall name =
      case List.find ((== name) . methodName) methods of
        Nothing -> R.Failure $ "method '" <> T.unpack name <> "' not found"
        Just m  -> R.Success $ methodBody m args

    intCall ix =
      case drop ix methods of
        []  -> R.Failure $ "method #" <> show ix <> " not found"
        m:_ -> R.Success $ methodBody m args


ignoreParseError :: Applicative m => ParseError -> m ()
ignoreParseError _ = pure ()


-- | Start RPC server with a set of RPC methods.
serve
  :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
  => Int        -- ^ Port number
  -> [Method m] -- ^ list of methods
  -> m ()
serve port methods =
  runGeneralTCPServer settings $ \ad -> do
    (rsrc, _) <- appSource ad $$+ return ()
    processRequests methods rsrc (appSink ad) `catch` ignoreParseError

  where
    settings =
      setAfterBind
        (\s -> setSocketOption s ReuseAddr 1)
        (serverSettings port "*")
