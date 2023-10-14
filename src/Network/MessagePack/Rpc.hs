{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module Network.MessagePack.Rpc
  ( I.Doc (..)
  , I.Returns
  , I.ReturnsM
  , method
  , rpc
  , docs
  , stubs, Rpc, RpcT (local)
  ) where

import           Control.Monad.Catch              (MonadThrow)
import           Data.Kind                        (Type)
import           Data.Text                        (Text)

import qualified Network.MessagePack.Interface    as I
import qualified Network.MessagePack.Types.Client as Client
import qualified Network.MessagePack.Types.Server as Server

-- Import orphan instances for RpcType and IsReturnType.
-- TODO(SX91): Avoid orphan instances. See issue #7.
import           Network.MessagePack.Client.Basic ()
import           Network.MessagePack.Server.Basic ()


class RpcService rpc where
  type ClientMonad rpc :: Type -> Type
  type ServerMonad rpc :: Type -> Type
  type F rpc
  rpc    :: rpc -> I.ClientType (ClientMonad rpc) (F rpc)
  method :: rpc -> Server.Method (ServerMonad rpc)
  docs   :: rpc -> (Text, I.Doc (F rpc))


type Rpc f = RpcT IO IO f

data RpcT mc ms f = RpcT
  { rpcPure    :: !(I.ClientType mc f)
  , local      :: !(I.HaskellType f)
  , methodPure :: !(Server.Method ms)
  , intfPure   :: !(I.Interface f)
  }

instance forall mc ms (f :: Type). RpcService (RpcT mc ms f) where
  type ClientMonad (RpcT mc ms f) = mc
  type ServerMonad (RpcT mc ms f) = ms
  type F (RpcT mc ms f) = f
  rpc    = rpcPure
  {-# INLINE rpc #-}
  method = methodPure
  {-# INLINE method #-}
  docs r = (Server.methodName $ method r, I.docs $ intfPure r)
  {-# INLINE docs #-}


stubs
  :: ( Client.RpcType (I.ClientType mc f)
     , Server.MethodType ms (I.ServerType ms f)
     , I.IsReturnType ms f
     , I.IsDocType f
     , MonadThrow ms
     )
  => Text -> I.Doc f -> I.HaskellType f -> RpcT mc ms f
stubs n doc f = RpcT c f m i
  where
    c = Client.call n
    m = I.method i f
    i = I.interface n doc
