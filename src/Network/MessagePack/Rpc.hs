{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE TypeFamilies          #-}
module Network.MessagePack.Rpc
  ( I.Returns
  , I.Doc (..)
  , method
  , rpc
  , docs
  , stubs, Rpc, RpcT (local)
  , stubsIO, RpcIO, RpcIOT (localIO)
  ) where

import           Control.Monad.Catch                    (MonadThrow)
import           Data.Text                              (Text)

import qualified Network.MessagePack.Interface.Internal as I
import qualified Network.MessagePack.Types.Client       as Client
import qualified Network.MessagePack.Types.Server       as Server

-- Import orphan instances for RpcType and IsReturnType.
-- TODO(SX91): Avoid orphan instances. See issue #7.
import           Network.MessagePack.Client.Basic       ()
import           Network.MessagePack.Server.Basic       ()


class RpcService rpc where
  type ClientMonad rpc :: * -> *
  type ServerMonad rpc :: * -> *
  type F rpc
  rpc    :: rpc -> I.ClientType (ClientMonad rpc) (F rpc)
  method :: rpc -> Server.Method (ServerMonad rpc)
  docs   :: rpc -> (Text, I.Doc (F rpc))


--------------------------------------------------------------------------------
--
-- :: Non-IO RPCs
--
--------------------------------------------------------------------------------

type Rpc f = RpcT IO IO f

data RpcT mc ms f = RpcT
  { rpcPure    :: !(I.ClientType mc f)
  , local      :: !(I.HaskellType f)
  , methodPure :: !(Server.Method ms)
  , intfPure   :: !(I.Interface f)
  }

instance RpcService (RpcT mc ms f) where
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


--------------------------------------------------------------------------------
--
-- :: IO RPCs
--
--------------------------------------------------------------------------------

type RpcIO f = RpcIOT IO IO f

data RpcIOT mc ms f = RpcIOT
  { rpcIO    :: !(I.ClientType mc f)
  , localIO  :: !(I.HaskellTypeIO f)
  , methodIO :: !(Server.Method ms)
  , intfIO   :: !(I.Interface f)
  }

instance RpcService (RpcIOT mc ms f) where
  type ClientMonad (RpcIOT mc ms f) = mc
  type ServerMonad (RpcIOT mc ms f) = ms
  type F (RpcIOT mc ms f) = f
  rpc    = rpcIO
  {-# INLINE rpc #-}
  method = methodIO
  {-# INLINE method #-}
  docs r = (Server.methodName $ method r, I.docs $ intfIO r)
  {-# INLINE docs #-}


stubsIO
  :: ( Client.RpcType (I.ClientType mc f)
     , Server.MethodType ms (I.ServerTypeIO ms f)
     , I.IsReturnTypeIO ms f
     , I.IsDocType f
     , MonadThrow ms
     )
  => Text -> I.Doc f -> I.HaskellTypeIO f -> RpcIOT mc ms f
stubsIO n doc f = RpcIOT c f m i
  where
    c = Client.call n
    m = I.methodIO i f
    i = I.interface n doc
