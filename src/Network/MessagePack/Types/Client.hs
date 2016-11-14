module Network.MessagePack.Types.Client
  ( RpcType (..)
  , call
  ) where

import           Data.MessagePack (Object)
import           Data.Text        (Text)


class RpcType r where
  rpcc :: Text -> [Object] -> r


-- | Call an RPC Method
call :: RpcType a => Text -> a
call name = rpcc name []
{-# INLINE call #-}
