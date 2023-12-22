{-# LANGUAGE ScopedTypeVariables #-}
module Network.MessagePack.Client (
  -- * MessagePack Client type
    Basic.Client
  , Basic.ClientT
  , Basic.execClient
  , runClient

  -- * Call RPC method
  , Basic.call

  -- * RPC error
  , Basic.RpcError (..)
  , Basic.RpcType
  ) where

import           Control.Monad                       (when)
import           Control.Monad.Catch                 (catch)
import qualified Data.ByteString                     as S
import           Data.Default.Class                  (Default (..))

import           Network.MessagePack.Capabilities    (ClientCapability (..),
                                                      ServerCapability (..))
import qualified Network.MessagePack.Client.Basic    as Basic
import qualified Network.MessagePack.Client.Internal as Internal
import qualified Network.MessagePack.Protocol        as Protocol


useDefault :: (Applicative m, Default a) => Basic.RpcError -> m a
useDefault _ = pure def


initClient :: Basic.Client ()
initClient = do
  caps <- Protocol.capabilitiesC [CCapMethodList] `catch` useDefault
  when (SCapMethodList `elem` caps) $ do
    mths <- Protocol.methodListC
    Internal.setMethodList mths


runClient :: S.ByteString -> Int -> Basic.Client a -> IO a
runClient host port client =
  Basic.execClient host port (initClient >> client)
