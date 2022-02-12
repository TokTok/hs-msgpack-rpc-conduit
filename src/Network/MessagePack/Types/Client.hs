{-# LANGUAGE Safe #-}
module Network.MessagePack.Types.Client
  ( RpcType (..)
  , call
  ) where

import           Data.MessagePack.Types (MessagePack, Object, defaultConfig,
                                         toObject)
import           Data.Text              (Text)


class RpcType r where
  rpcc :: Text -> [Object] -> r


instance (MessagePack o, RpcType r) => RpcType (o -> r) where
  rpcc name args arg = rpcc name (toObject defaultConfig arg : args)
  {-# INLINE rpcc #-}


-- | Call an RPC Method
call :: RpcType a => Text -> a
call name = rpcc name []
{-# INLINE call #-}
