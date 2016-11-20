{-# LANGUAGE Trustworthy #-}
module Network.MessagePack.Internal.TypeUtil where

import qualified Data.List.Utils as List
import           Data.Typeable   (Typeable)
import qualified Data.Typeable   as Typeable


typeName :: Typeable a => a -> String
typeName = List.replace "[Char]" "String" . show . Typeable.typeOf
