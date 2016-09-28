{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Network.MessagePack.Rpc
  ( I.Returns
  , I.Doc (..)
  , method
  , rpc
  , docs
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
  rpc    :: rpc -> I.ClientType (M rpc) (F rpc)
  method :: rpc -> Server.Method (M rpc)
  docs   :: rpc -> (String, I.Doc (F rpc))


--------------------------------------------------------------------------------
--
-- :: Non-IO RPCs
--
--------------------------------------------------------------------------------


data Rpc m f = Rpc
  { rpcPure    :: I.ClientType m f
  , local      :: I.HaskellType f
  , methodPure :: Server.Method m
  , intfPure   :: I.Interface f
  }

instance RpcService (Rpc m f) where
  type M (Rpc m f) = m
  type F (Rpc m f) = f
  rpc    = rpcPure
  method = methodPure
  docs r = (Server.methodName $ method r, I.docs $ intfPure r)


stubs
  :: ( Client.RpcType (I.ClientType m f)
     , Server.MethodType m (I.ServerType m f)
     , I.IsReturnType m f
     , I.IsDocType f
     , MonadThrow m
     )
  => String -> I.Doc f -> I.HaskellType f -> Rpc m f
stubs n doc f = Rpc c f m i
  where
    c = Client.call n
    m = I.method i f
    i = I.interface n doc


--------------------------------------------------------------------------------
--
-- :: IO RPCs
--
--------------------------------------------------------------------------------


data RpcIO m f = RpcIO
  { rpcIO    :: I.ClientType m f
  , localIO  :: I.HaskellTypeIO f
  , methodIO :: Server.Method m
  , intfIO   :: I.Interface f
  }

instance RpcService (RpcIO m f) where
  type M (RpcIO m f) = m
  type F (RpcIO m f) = f
  rpc    = rpcIO
  method = methodIO
  docs r = (Server.methodName $ method r, I.docs $ intfIO r)


stubsIO
  :: ( Client.RpcType (I.ClientType m f)
     , Server.MethodType m (I.ServerTypeIO m f)
     , I.IsReturnTypeIO m f
     , I.IsDocType f
     , MonadThrow m
     )
  => String -> I.Doc f -> I.HaskellTypeIO f -> RpcIO m f
stubsIO n doc f = RpcIO c f m i
  where
    c = Client.call n
    m = I.methodIO i f
    i = I.interface n doc
