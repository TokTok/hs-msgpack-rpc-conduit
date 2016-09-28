{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.MessagePack.Protocol where

import           Control.Applicative              (Applicative, pure)
import           Control.Monad.Catch              (MonadCatch)
import           Control.Monad.Trans              (MonadIO)
import           Control.Monad.Trans.Control      (MonadBaseControl)

import           Network.MessagePack.Capabilities
import           Network.MessagePack.Client.Basic
import           Network.MessagePack.Server.Basic


capabilitiesN :: String
capabilitiesN = "rpc.capabilities"

capabilitiesC :: [ClientCapability] -> Client [ServerCapability]
capabilitiesC = call capabilitiesN

capabilitiesS
  :: Applicative m
  => [Method m]
  -> [ClientCapability]
  -> ServerT m [ServerCapability]
capabilitiesS _ _ = pure [SCapMethodList]


methodListN :: String
methodListN = "rpc.methodList"

methodListC :: Client [String]
methodListC = call methodListN

methodListS
  :: Applicative m
  => [Method m]
  -> ServerT m [String]
methodListS = pure . map methodName


protocolMethods
  :: (MonadBaseControl IO m, MonadIO m, MonadCatch m)
  => [Method m]
  -> [Method m]
protocolMethods methods = methods ++
  [ method capabilitiesN (MethodDocs [MethodVal "clientCaps" "ClientCapability"] (MethodVal "serverCaps" "ServerCapability"))
      (capabilitiesS methods)
  , method methodListN   (MethodDocs [] (MethodVal "names" "[String]"))
      (methodListS   methods)
  ]
