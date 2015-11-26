{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveFoldable #-}
module SList where

import Data.Monoid ((<>))
import GHC.Exts (IsList(..))

data SList a =
    SCons !a !(SList a)
  | SNil
  deriving (Show, Eq, Functor, Foldable)

instance Applicative SList where
    SCons f fs <*> SCons x xs = SCons (f x) (fs <*> xs)
    _          <*> SNil       = SNil
    SNil       <*> _          = SNil

    pure x = SCons x SNil

instance Monad SList where
    x >>= f = sjoin (fmap f x)
    return = pure

instance Monoid (SList a) where
    mempty = SNil
    mappend SNil         ys = ys
    mappend (SCons x xs) ys = SCons x (mappend xs ys)

instance IsList (SList a) where
    type Item (SList a) = a

    fromList = foldr SCons SNil

    toList (SCons x xs) = x : toList xs
    toList SNil         = []

sjoin :: SList (SList a) -> SList a
sjoin SNil           = SNil
sjoin (SCons xs xss) = xs <> sjoin xss
