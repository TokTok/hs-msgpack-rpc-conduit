{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe              #-}
module Network.MessagePack.Internal.TypeUtil
  ( typeName
  ) where

import qualified Data.List.Utils as List
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)
import qualified Data.Typeable   as Typeable


typeName :: Typeable a => a -> T.Text
typeName = T.replace "[Char]" "String" . T.pack . show . Typeable.typeOf
