{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData         #-}
module Network.MessagePack.Types.Error
  ( RpcError (..)
  , ServerError (..)
  ) where

import           Control.Exception (Exception)
import           Data.MessagePack  (Object)
import           Data.Text         (Text)
import           Data.Typeable     (Typeable)


-- | RPC error type
data RpcError
  = RemoteError Object          -- ^ Server error
  | ResultTypeError Text Object -- ^ Result type mismatch
  | ProtocolError Text          -- ^ Protocol error
  deriving (Show, Eq, Ord, Typeable)

instance Exception RpcError


newtype ServerError = ServerError Text
  deriving (Show, Typeable)

instance Exception ServerError
