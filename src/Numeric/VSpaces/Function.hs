module Numeric.VSpaces.Function where

import Prelude

import Numeric.Ring
import Numeric.Module

data Linear s a b = (Module s a => Module s b) => Linear (Module s a => a -> b)

-- | Linear maps and vector spaces form a category
instance Category (Linear s)
  where
  id = Linear id
  Linear f . Linear g = Linear $ f . g

-- | Linear maps inherit a pointwise abelian group structure from their codomain
instance Module s b => Semigroup (Add (Linear s a b))
  where
  Add (Linear x) <> Add (Linear y) = Add $ Linear $ \a -> x a + y a

instance Module s b => Commutative (Add (Linear s a b))

instance Module s b => Monoid (Add (Linear s a b))
  where
  mempty = Add $ Linear $ const zero

instance Module s b => Group (Add (Linear s a b))
  where
  invert (Add (Linear f)) = Add $ Linear $ getAdd . invert . Add . f

-- | The abelian group of R-linear maps is a full module when R is a commutative ring
instance (Commutative (Mul s), Module s b) => Module s (Linear s a b)
  where
  s |*| Linear f = Linear $ (s |*|) . f

instance (Commutative (Mul s), Module s a, Module s b) => Hom s (Linear s a b) a b
  where
  Linear f |$| a = f a

-- | The set of endomorphisms on a given module forms a ring
instance Module s a => Semigroup (Mul (Linear s a a))
  where
  Mul f <> Mul g = Mul $ f . g

instance Module s a => Monoid (Mul (Linear s a a))
  where
  mempty = Mul $ id

instance Module s a => Semiring (Linear s a a)
