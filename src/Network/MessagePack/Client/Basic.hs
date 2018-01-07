{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy         #-}

-------------------------------------------------------------------
-- |
-- Module    : Network.MessagePackRpc.Client
-- Copyright : (c) Hideyuki Tanaka, 2010-2015
-- License   : BSD3
--
-- Maintainer:  Hideyuki Tanaka <tanaka.hideyuki@gmail.com>
-- Stability :  experimental
-- Portability: portable
--
-- This module is client library of MessagePack-RPC.
-- The specification of MessagePack-RPC is at
-- <http://redmine.msgpack.org/projects/msgpack/wiki/RPCProtocolSpec>.
--
-- A simple example:
--
-- > import Network.MessagePackRpc.Client
-- >
-- > add :: Int -> Int -> Client Int
-- > add = call "add"
-- >
-- > main = execClient "localhost" 5000 $ do
-- >   ret <- add 123 456
-- >   liftIO $ print ret
--
--------------------------------------------------------------------

module Network.MessagePack.Client.Basic (
  -- * MessagePack Client type
    Client
  , ClientT
  , execClient

  -- * Call RPC method
  , call

  -- * RPC error
  , RpcError (..)
  , RpcType (..)
  ) where

import           Control.Monad.Catch                 (MonadThrow, throwM)
import qualified Control.Monad.State.Strict          as CMS
import qualified Data.ByteString                     as S
import           Data.Conduit                        (($$+))
import           Data.Conduit.Network                (appSink, appSource,
                                                      clientSettings,
                                                      runTCPClient)
import           Data.MessagePack                    (MessagePack, Object,
                                                      fromObject, toObject)
import qualified Data.MessagePack.Types.Result       as R
import           Data.Text                           (Text)
import qualified Data.Text                           as T

import           Network.MessagePack.Client.Internal
import           Network.MessagePack.Types.Client
import           Network.MessagePack.Types.Error


execClient :: S.ByteString -> Int -> Client a -> IO a
execClient host port client =
  runTCPClient (clientSettings port host) $ \ad -> do
    (rsrc, _) <- appSource ad $$+ return ()
    CMS.evalStateT (runClientT client) Connection
      { connSource = rsrc
      , connSink   = appSink ad
      , connMsgId  = 0
      , connMths   = []
      }
