{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
module Network.MessagePack.Interface.Internal where

import           Control.Monad.Catch                   (MonadThrow)
import           Data.Text                             (Text)
import           Data.Typeable                         (Typeable)

import qualified Network.MessagePack.Internal.TypeUtil as TypeUtil
import qualified Network.MessagePack.Types.Client      as Client
import           Network.MessagePack.Types.Server      (Method, MethodDocs (..),
                                                        MethodVal (..))
import qualified Network.MessagePack.Types.Server      as Server


data Returns r


data Interface f = Interface
  { name :: !Text
  , docs :: !(Doc f)
  }


newtype InterfaceM (m :: * -> *) f = InterfaceM
  { nameM :: !Text
  }


interface :: Text -> Doc f -> Interface f
interface = Interface


concrete :: Interface f -> InterfaceM m f
concrete = InterfaceM . name


coerce :: InterfaceM m a -> InterfaceM m b
coerce = InterfaceM . nameM


--------------------------------------------------------------------------------
--
-- :: Documentation
--
--------------------------------------------------------------------------------


class IsDocType f where
  data Doc f
  flatDoc :: Doc f -> MethodDocs

instance Typeable r => IsDocType (Returns r) where
  data Doc (Returns r) = Ret Text
    deriving (Eq, Read, Show)
  flatDoc (Ret x) =
    let typeName = TypeUtil.typeName (undefined :: r) in
    MethodDocs [] (MethodVal x typeName)

instance (Typeable o, IsDocType r) => IsDocType (o -> r) where
  data Doc (o -> r) = Arg Text (Doc r)
  flatDoc (Arg o r) =
    let doc = flatDoc r in
    let typeName = TypeUtil.typeName (undefined :: o) in
    doc { methodArgs = MethodVal o typeName : methodArgs doc }

deriving instance Eq   (Doc r) => Eq   (Doc (o -> r))
deriving instance Read (Doc r) => Read (Doc (o -> r))
deriving instance Show (Doc r) => Show (Doc (o -> r))


--------------------------------------------------------------------------------
--
-- :: Client
--
--------------------------------------------------------------------------------


class IsClientType (m :: * -> *) f where
  type ClientType m f

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


instance IsReturnType m r => IsReturnType m (o -> r) where
  type HaskellType (o -> r) = o -> HaskellType r
  type ServerType m (o -> r) = o -> ServerType m r

  implement i f a = next (coerce i) (f a)
    where
      next :: InterfaceM m r -> HaskellType r -> ServerType m r
      next = implement


methodM
  :: ( Server.MethodType m (ServerType m f)
     , IsDocType f
     , IsReturnType m f
     , MonadThrow m
     )
  => InterfaceM m f -> Doc f -> HaskellType f -> Method m
methodM i doc f = Server.method (nameM i) (flatDoc doc) (implement i f)


method
  :: ( MonadThrow m
     , Server.MethodType m (ServerType m f)
     , IsDocType f
     , IsReturnType m f)
  => Interface f -> HaskellType f -> Method m
method i = methodM (concrete i) (docs i)


--------------------------------------------------------------------------------
--
-- :: IO server
--
--------------------------------------------------------------------------------


class IsReturnTypeIO (m :: * -> *) f where
  type HaskellTypeIO f
  type ServerTypeIO m f

  implementIO :: InterfaceM m f -> HaskellTypeIO f -> ServerTypeIO m f

instance IsReturnTypeIO m r => IsReturnTypeIO m (o -> r) where
  type HaskellTypeIO (o -> r) = o -> HaskellTypeIO r
  type ServerTypeIO m (o -> r) = o -> ServerTypeIO m r

  implementIO i f a = next (coerce i) (f a)
    where
      next :: InterfaceM m r -> HaskellTypeIO r -> ServerTypeIO m r
      next = implementIO


methodIOM
  :: ( Server.MethodType m (ServerTypeIO m f)
     , IsDocType f
     , IsReturnTypeIO m f
     )
  => InterfaceM m f -> Doc f -> HaskellTypeIO f -> Method m
methodIOM i doc f = Server.method (nameM i) (flatDoc doc) (implementIO i f)


methodIO
  :: ( Server.MethodType m (ServerTypeIO m f)
     , IsDocType f
     , IsReturnTypeIO m f)
  => Interface f -> HaskellTypeIO f -> Method m
methodIO i = methodIOM (concrete i) (docs i)
