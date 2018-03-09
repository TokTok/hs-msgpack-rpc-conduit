{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}
module Network.MessagePack.Client (
  -- * MessagePack Client type
    Client
  , ClientT
  , execClient
  , runClient

  -- * Call RPC method
  , call

  -- * RPC error
  , RpcError (..)
  , RpcType
  ) where

import           Control.Applicative                 (Applicative, pure)
import           Control.Monad                       (when)
import           Control.Monad.Catch                 (catch)
import qualified Data.ByteString                     as S
import           Data.Default.Class                  (Default (..))
import           Data.Default.Instances.Base         ()

import           Network.MessagePack.Capabilities
import           Network.MessagePack.Client.Basic
import qualified Network.MessagePack.Client.Internal as Internal
import qualified Network.MessagePack.Protocol        as Protocol


useDefault :: (Applicative m, Default a) => RpcError -> m a
useDefault _ = pure def


initClient :: Client ()
initClient = do
  caps <- Protocol.capabilitiesC [CCapMethodList] `catch` useDefault
  when (SCapMethodList `elem` caps) $ do
    mths <- Protocol.methodListC
    Internal.setMethodList mths


runClient :: S.ByteString -> Int -> Client a -> IO a
runClient host port client =
  execClient host port (initClient >> client)
