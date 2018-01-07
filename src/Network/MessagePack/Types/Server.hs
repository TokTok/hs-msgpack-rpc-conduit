{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Safe                  #-}
module Network.MessagePack.Types.Server
  ( MethodVal (..)
  , MethodDocs (..)
  , MethodType (..)
  , Method (..)
  , method
  ) where

import           Control.Monad    (Monad)
import           Data.MessagePack (Object)
import           Data.Text        (Text)


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


class Monad m => MethodType m f where
  -- | Create a RPC method from a Haskell function
  toBody :: Text -> f -> [Object] -> m Object


-- | Build a method
method
  :: MethodType m f
  => Text     -- ^ Method name
  -> MethodDocs
  -> f        -- ^ Method body
  -> Method m
method name docs body = Method name docs $ toBody name body
