module Network.MessagePack.Types.Client
  ( RpcType (..)
  , call
  ) where

import           Data.MessagePack (MessagePack (toObject), Object)
import           Data.Text        (Text)


class RpcType r where
  rpcc :: Text -> [Object] -> r


instance (MessagePack o, RpcType r) => RpcType (o -> r) where
  rpcc name args arg = rpcc name (toObject arg : args)
  {-# INLINE rpcc #-}


-- | Call an RPC Method
call :: RpcType a => Text -> a
call name = rpcc name []
{-# INLINE call #-}
