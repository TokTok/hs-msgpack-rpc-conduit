{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData        #-}
module Network.MessagePack.Types.Result
    ( Result (..)
    ) where

import           Control.Applicative (Alternative (..))

data Result a
    = Success a
    | Failure String
    deriving (Read, Show, Eq, Functor, Foldable, Traversable)

instance Applicative Result where
    pure = Success

    Success f   <*> x = fmap f x
    Failure msg <*> _ = Failure msg

instance Alternative Result where
    empty = Failure "empty alternative"

    s@Success {} <|> _ = s
    _            <|> r = r

instance Monad Result where
    Success x   >>= f = f x
    Failure msg >>= _ = Failure msg

    return = Success

#if (MIN_VERSION_base(4,13,0))
instance MonadFail Result where
#endif
    fail = Failure
