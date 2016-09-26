{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Network.MessagePack.Interface.Internal where

import           Control.Monad.Catch        (MonadThrow)
import           Control.Monad.Trans        (MonadIO, liftIO)

import           Network.MessagePack.Client (ClientT)
import qualified Network.MessagePack.Client as Client
import           Network.MessagePack.Server (Method, ServerT)
import qualified Network.MessagePack.Server as Server


data Returns r


data Interface f = Interface
  { name :: String
  }


data InterfaceM (m :: * -> *) f = InterfaceM
  { nameM :: String
  }


interface :: String -> Interface f
interface = Interface


concrete :: Interface f -> InterfaceM m f
concrete = InterfaceM . name


--------------------------------------------------------------------------------
--
-- :: Client
--
--------------------------------------------------------------------------------


class IsClientType (m :: * -> *) f where
  type ClientType m f

instance IsClientType m (Returns r) where
  type ClientType m (Returns r) = ClientT m r

instance IsClientType m r => IsClientType m (o -> r) where
  type ClientType m (o -> r) = o -> ClientType m r


call :: Client.RpcType (ClientType m f) => InterfaceM m f -> ClientType m f
call = Client.call . nameM


--------------------------------------------------------------------------------
--
-- :: Non-IO server
--
--------------------------------------------------------------------------------


class IsReturnType (m :: * -> *) f where
  type HaskellType f
  type ServerType m f

  implement :: InterfaceM m f -> HaskellType f -> ServerType m f

instance Monad m => IsReturnType m (Returns r) where
  type HaskellType (Returns r) = r
  type ServerType m (Returns r) = ServerT m r

  implement _ = return

instance IsReturnType m r => IsReturnType m (o -> r) where
  type HaskellType (o -> r) = o -> HaskellType r
  type ServerType m (o -> r) = o -> ServerType m r

  implement i f a = next InterfaceM { nameM = nameM i } (f a)
    where
      next :: InterfaceM m r -> HaskellType r -> ServerType m r
      next = implement


methodM
  :: ( Server.MethodType m (ServerType m f)
     , IsReturnType m f
     , MonadThrow m
     )
  => InterfaceM m f -> HaskellType f -> Method m
methodM i f = Server.method (nameM i) (implement i f)


method
  :: ( MonadThrow m
     , Server.MethodType m (ServerType m f)
     , IsReturnType m f)
  => Interface f -> HaskellType f -> Method m
method = methodM . concrete


--------------------------------------------------------------------------------
--
-- :: IO server
--
--------------------------------------------------------------------------------


class IsReturnTypeIO (m :: * -> *) f where
  type HaskellTypeIO f
  type ServerTypeIO m f

  implementIO :: InterfaceM m f -> HaskellTypeIO f -> ServerTypeIO m f

instance MonadIO m => IsReturnTypeIO m (Returns r) where
  type HaskellTypeIO (Returns r) = IO r
  type ServerTypeIO m (Returns r) = ServerT m r

  implementIO _ = liftIO

instance IsReturnTypeIO m r => IsReturnTypeIO m (o -> r) where
  type HaskellTypeIO (o -> r) = o -> HaskellTypeIO r
  type ServerTypeIO m (o -> r) = o -> ServerTypeIO m r

  implementIO i f a = next InterfaceM { nameM = nameM i } (f a)
    where
      next :: InterfaceM m r -> HaskellTypeIO r -> ServerTypeIO m r
      next = implementIO


methodIOM
  :: ( Server.MethodType m (ServerTypeIO m f)
     , IsReturnTypeIO m f
     )
  => InterfaceM m f -> HaskellTypeIO f -> Method m
methodIOM i f = Server.method (nameM i) (implementIO i f)


methodIO
  :: ( Server.MethodType m (ServerTypeIO m f)
     , IsReturnTypeIO m f)
  => Interface f -> HaskellTypeIO f -> Method m
methodIO = methodIOM . concrete
