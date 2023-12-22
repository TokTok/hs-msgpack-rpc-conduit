{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
module Network.MessagePack.Interface
    ( Interface (..)
    , InterfaceM (..)
    , IsDocType (..)
    , IsClientType (..)
    , IsReturnType (..)
    , Doc (..)
    , Returns
    , ReturnsM
    , call
    , concrete
    , interface
    , method
    ) where

import           Control.Monad.Catch              (MonadThrow)
import           Data.Kind                        (Type)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Typeable                    (Typeable)
import qualified Data.Typeable                    as Typeable

import qualified Network.MessagePack.Types.Client as Client
import           Network.MessagePack.Types.Server (Method, MethodDocs (..),
                                                   MethodVal (..))
import qualified Network.MessagePack.Types.Server as Server


data Interface f = Interface
  { name :: Text
  , docs :: Doc f
  }


newtype InterfaceM (m :: Type -> Type) f = InterfaceM
  { nameM :: Text
  }


interface :: Text -> Doc f -> Interface f
interface = Interface


concrete :: Interface f -> InterfaceM m f
concrete = InterfaceM . name


--------------------------------------------------------------------------------
--
-- :: Documentation
--
--------------------------------------------------------------------------------


class IsDocType f where
  data Doc f
  flatDoc :: Doc f -> MethodDocs

data Returns r

instance Typeable (r :: Type) => IsDocType (Returns r) where
  data Doc (Returns r) = Ret Text
    deriving (Eq, Read, Show)
  flatDoc (Ret retName) =
    MethodDocs [] (MethodVal retName (typeName (undefined :: r)))

data ReturnsM (m :: Type -> Type) r

instance Typeable (r :: Type) => IsDocType (ReturnsM m r) where
  data Doc (ReturnsM m r) = RetM Text
    deriving (Eq, Read, Show)
  flatDoc (RetM retName) =
    MethodDocs [] (MethodVal retName (typeName (undefined :: r)))

instance (Typeable o, IsDocType r) => IsDocType (o -> r) where
  data Doc (o -> r) = Arg Text (Doc r)
  flatDoc (Arg o r) =
    let doc = flatDoc r in
    let ty = typeName (undefined :: o) in
    doc { methodArgs = MethodVal o ty : methodArgs doc }

deriving instance Eq   (Doc r) => Eq   (Doc (o -> r))
deriving instance Read (Doc r) => Read (Doc (o -> r))
deriving instance Show (Doc r) => Show (Doc (o -> r))


typeName :: Typeable a => a -> Text
typeName = Text.replace "[Char]" "String" . Text.pack . show . Typeable.typeOf


--------------------------------------------------------------------------------
--
-- :: Client
--
--------------------------------------------------------------------------------


class IsClientType (m :: Type -> Type) f where
  type ClientType m f

instance IsClientType m r => IsClientType m (o -> r) where
  type ClientType m (o -> r) = o -> ClientType m r


call :: Client.RpcType (ClientType m f) => InterfaceM m f -> ClientType m f
call = Client.call . nameM


--------------------------------------------------------------------------------
--
-- :: Server
--
--------------------------------------------------------------------------------


class IsReturnType (m :: Type -> Type) f where
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

      coerce :: InterfaceM m a -> InterfaceM m b
      coerce = InterfaceM . nameM


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
