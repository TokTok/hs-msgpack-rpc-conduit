{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}
module Network.MessagePack.Capabilities
    ( ServerCapability (..)
    , ClientCapability (..)
    ) where

import           Data.MessagePack (MessagePack)
import           GHC.Generics     (Generic)


data ServerCapability
    = SCapMethodList
      -- ^ Server supports method lists and can handle more efficient method codes
      -- instead of strings for names. It supports the "internal.methodList" call
      -- to return an ordered list of method names. The client can send an index
      -- in this list instead of the name itself when performing an RPC call.
    deriving (Eq, Generic)

instance MessagePack ServerCapability


data ClientCapability
    = CCapMethodList
      -- ^ Client supports method lists and can send more efficient method codes
      -- instead of strings for names.
    deriving (Eq, Generic)

instance MessagePack ClientCapability
