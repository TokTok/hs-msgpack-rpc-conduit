{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.MessagePack.Server (
  -- * RPC method types
    Method
  , MethodType (..)
  , MethodDocs (..)
  , MethodVal (..)
  , ServerT (..)
  , Server

  -- * Build a method
  , method
  , methodName
  , methodDocs

  -- * Start RPC server
  , serve
  , runServer
  ) where

import           Control.Monad.Catch              (MonadCatch)
import           Control.Monad.IO.Unlift          (MonadUnliftIO)
import           Control.Monad.Trans              (MonadIO)
import           Control.Monad.Trans.Control      (MonadBaseControl)

import           Network.MessagePack.Protocol     (protocolMethods)
import           Network.MessagePack.Server.Basic


-- | Start RPC server with a set of RPC methods.
runServer
  :: (MonadBaseControl IO m, MonadIO m, MonadCatch m, MonadUnliftIO m)
  => Int        -- ^ Port number
  -> [Method m] -- ^ list of methods
  -> m ()
runServer port methods =
  serve port (protocolMethods methods)
