{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.MessagePack.Client.Internal where

import           Control.Applicative               (Applicative)
import           Control.Monad                     (when)
import           Control.Monad.Catch               (MonadCatch, MonadThrow,
                                                    throwM)
import qualified Control.Monad.State.Strict        as CMS
import           Control.Monad.Validate            (runValidate)
import qualified Data.Binary                       as Binary
import qualified Data.ByteString                   as S
import           Data.Conduit                      (ConduitT, SealedConduitT,
                                                    Void, runConduit, ($$++),
                                                    (.|))
import qualified Data.Conduit.Binary               as CB
import           Data.Conduit.Serialization.Binary (sinkGet)
import           Data.Kind                         (Type)
import           Data.MessagePack                  (MessagePack, Object,
                                                    defaultConfig, fromObject,
                                                    fromObjectWith)
import           Data.Monoid                       ((<>))
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Network.MessagePack.Types.Result  as R

import           Network.MessagePack.Interface     (IsClientType (..), Returns,
                                                    ReturnsM)
import           Network.MessagePack.Types.Client
import           Network.MessagePack.Types.Error
import           Network.MessagePack.Types.Spec


-- | RPC connection type
data Connection m = Connection
  { connSource :: !(SealedConduitT () S.ByteString m ())
  , connSink   :: !(ConduitT S.ByteString Void m ())
  , connMsgId  :: !Int
  , connMths   :: ![Text]
  }


newtype ClientT m a
  = ClientT { runClientT :: CMS.StateT (Connection m) m a }
  deriving (Functor, Applicative, Monad, CMS.MonadIO, MonadThrow, MonadCatch)

type Client a = ClientT IO a

instance forall m (r :: Type). IsClientType m (Returns r) where
  type ClientType m (Returns r) = ClientT m r

instance forall m io (r :: Type). IsClientType m (ReturnsM io r) where
  type ClientType m (ReturnsM io r) = ClientT m r


instance (CMS.MonadIO m, MonadThrow m, MessagePack o)
    => RpcType (ClientT m o) where
  rpcc name args = do
    res <- rpcCall name (reverse args)
    case runValidate $ fromObjectWith defaultConfig res of
      Right ok  ->
        return ok
      Left err ->
        throwM $ ResultTypeError (T.pack $ show err) res


rpcCall :: (MonadThrow m, CMS.MonadIO m) => Text -> [Object] -> ClientT m Object
rpcCall methodName args = ClientT $ do
  conn <- CMS.get
  let msgid = connMsgId conn

  (rsrc', res) <- CMS.lift $ do
    let req = packRequest (connMths conn) (0, msgid, methodName, args)
    runConduit $ CB.sourceLbs req .| connSink conn
    connSource conn $$++ sinkGet Binary.get

  CMS.put conn
    { connSource = rsrc'
    , connMsgId  = msgid + 1
    }

  case unpackResponse res of
    Left err -> throwM $ ProtocolError $ "invalid response data: " <> T.pack (show err)
    Right (rtype, rmsgid, rerror, rresult) -> do
      when (rtype /= 1) $
        throwM $ ProtocolError $
          "invalid response type (expect 1, but got " <> T.pack (show rtype) <> "): " <> T.pack (show res)

      when (rmsgid /= msgid) $
        throwM $ ProtocolError $
          "message id mismatch: expect " <> T.pack (show msgid) <> ", but got " <> T.pack (show rmsgid)

      case fromObject rerror of
        Nothing -> throwM $ RemoteError rerror
        Just () -> return rresult


setMethodList :: Monad m => [Text] -> ClientT m ()
setMethodList mths = ClientT $ do
  conn <- CMS.get
  CMS.put conn { connMths = mths }
