module Network.MessagePack.Types.Spec
  ( Request
  , Response
  , packRequest
  , packResponse
  , unpackRequest
  , unpackResponse
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.List            as List
import           Data.MessagePack     (MessagePack, Object, fromObject, pack)

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
