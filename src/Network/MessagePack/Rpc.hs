{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Network.MessagePack.Rpc
  ( I.Returns
  , method
  , rpc
  , stubs, Rpc (local)
  , stubsIO, RpcIO (localIO)
  ) where

import           Control.Monad.Catch                    (MonadThrow)

import qualified Network.MessagePack.Client             as Client
import qualified Network.MessagePack.Interface.Internal as I
import qualified Network.MessagePack.Server             as Server


class RpcService rpc where
  type M rpc :: * -> *
  type F rpc
  rpc :: rpc -> I.ClientType (M rpc) (F rpc)
  method :: rpc -> Server.Method (M rpc)


--------------------------------------------------------------------------------
--
-- :: Non-IO RPCs
--
--------------------------------------------------------------------------------


data Rpc m f = Rpc
  { rpcPure    :: I.ClientType m f
  , local      :: I.HaskellType f
  , methodPure :: Server.Method m
  , _intf      :: I.InterfaceM m f
  }

instance RpcService (Rpc m f) where
  type M (Rpc m f) = m
  type F (Rpc m f) = f
  rpc = rpcPure
  method = methodPure


stubs
  :: ( Client.RpcType (I.ClientType m f)
     , Server.MethodType m (I.ServerType m f)
     , I.IsReturnType m f
     , MonadThrow m
     )
  => String -> I.HaskellType f -> Rpc m f
stubs n f = Rpc c f m (I.concrete i)
  where
    c = Client.call n
    m = I.method i f
    i = I.interface n


--------------------------------------------------------------------------------
--
-- :: IO RPCs
--
--------------------------------------------------------------------------------


data RpcIO m f = RpcIO
  { rpcIO    :: I.ClientType m f
  , localIO  :: I.HaskellTypeIO f
  , methodIO :: Server.Method m
  , _intfIO  :: I.InterfaceM m f
  }

instance RpcService (RpcIO m f) where
  type M (RpcIO m f) = m
  type F (RpcIO m f) = f
  rpc = rpcIO
  method = methodIO


stubsIO
  :: ( Client.RpcType (I.ClientType m f)
     , Server.MethodType m (I.ServerTypeIO m f)
     , I.IsReturnTypeIO m f
     , MonadThrow m
     )
  => String -> I.HaskellTypeIO f -> RpcIO m f
stubsIO n f = RpcIO c f m (I.concrete i)
  where
    c = Client.call n
    m = I.methodIO i f
    i = I.interface n
