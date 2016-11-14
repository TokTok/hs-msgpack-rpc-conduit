{-# LANGUAGE DeriveDataTypeable #-}
module Network.MessagePack.Types where

import           Control.Exception    (Exception)
import qualified Data.ByteString.Lazy as L
import qualified Data.List            as List
import           Data.MessagePack     (MessagePack, Object, fromObject, pack)
import           Data.Text            (Text)
import           Data.Typeable        (Typeable)


type Request ix = (Int, Int, ix, [Object])
type Response   = (Int, Int, Object, Object)

packRequest :: (Eq mth, MessagePack mth) => [mth] -> Request mth -> L.ByteString
packRequest [] req = pack req
packRequest mths req@(rtype, msgid, mth, obj) =
  case List.elemIndex mth mths of
    Nothing -> pack req
    Just ix -> pack (rtype, msgid, ix, obj)


packResponse :: Response -> L.ByteString
packResponse = pack

unpackResponse :: Object -> Maybe Response
unpackResponse = fromObject

unpackRequest :: MessagePack ix => Object -> Maybe (Request ix)
unpackRequest = fromObject


-- | RPC error type
data RpcError
  = RemoteError !Object           -- ^ Server error
  | ResultTypeError !Text !Object -- ^ Result type mismatch
  | ProtocolError !Text           -- ^ Protocol error
  deriving (Show, Eq, Ord, Typeable)

instance Exception RpcError


newtype ServerError = ServerError Text
  deriving (Show, Typeable)

instance Exception ServerError
