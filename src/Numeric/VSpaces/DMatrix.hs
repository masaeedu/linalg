{-# LANGUAGE InstanceSigs, ImpredicativeTypes #-}
module Numeric.VSpaces.DMatrix where

import Prelude

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Control.Monad (join)

import Numeric.Ring
import Numeric.Module

data DMatrix s j i = (FDim s j => FDim s i) => DMatrix { runDMatrix :: FDim s j => Vector s }

deriving instance (Show s, FDim s j) => Show (DMatrix s j i)

-- {{{ VECTOR FINAGLING

-- | Given a number of rows and a number of columns, generates an array representing a matrix of given dimension
matrix :: Int -> Int -> (Int -> Int -> x) -> Vector x
matrix n m f = join $ V.generate n $ \i -> V.generate m $ \j -> f i j

-- | Retrieves a row from a matrix of specified dimension
row :: Int -> Int -> Int -> Vector x -> Vector x
row n m i = V.slice (i * n) m

-- | Retrieves a column from a matrix of specified dimension
col :: Int -> Int -> Int -> Vector x -> Vector x
col n m j v = V.generate n $ \i -> v V.! (i * m + j)

-- | Generates a matrix of specified dimension containing a given value at a given cell
cell :: Monoid (Add x) => Int -> Int -> Int -> Int -> x -> Vector x
cell n m i j v = matrix n m $ \i' j' -> if i == i' && j == j' then v else zero

-- | Produces the dot product of two vectors
dotproduct :: (Monoid (Add r), Monoid (Mul r)) => Vector r -> Vector r -> r
dotproduct x y = getAdd $ foldMap Add $ V.zipWith (*) x y

-- }}}

-- | Finite-dimensional linear maps form a category
instance Category (DMatrix s)
  where
  id :: forall a. DMatrix s a a
  id = DMatrix $
    let
    n = dimension (Proxy @a)
    in
    matrix n n $ \i j -> if i == j then one else zero

  (.) :: forall j x i. DMatrix s x i -> DMatrix s j x -> DMatrix s j i
  DMatrix ix . DMatrix xj = DMatrix $
    let
    i = dimension @s (Proxy @i)
    j = dimension @s (Proxy @j)
    x = dimension @s (Proxy @x)
    in
    matrix i j $ \i' j' -> dotproduct (row i x i' ix) (col x j j' xj)

-- | Finite-dimensional linear maps inherit a pointwise abelian group structure from their codomain
instance FDim s i => Semigroup (Add (DMatrix s j i))
  where
  Add (DMatrix a) <> Add (DMatrix b) = Add $ DMatrix $ V.zipWith (+) a b

instance FDim s i => Commutative (Add (DMatrix s j i))

instance FDim s i => Monoid (Add (DMatrix s j i))
  where
  mempty = Add $ DMatrix $ V.replicate (dimension (Proxy @i) * dimension (Proxy @j)) zero

instance FDim s i => Group (Add (DMatrix s j i))
  where
  invert (Add (DMatrix v)) = Add $ DMatrix $ fmap negate v

-- | The abelian group of finite-dimensional R-linear maps is a full module when R is a commutative ring
instance (Commutative (Mul s), FDim s i) => Module s (DMatrix s j i)
  where
  s |*| DMatrix v = DMatrix $ fmap (s *) v

instance (Commutative (Mul s), FDim s i, FDim s j) => Hom s (DMatrix s j i) j i
  where
  m |$| v = build $ runDMatrix $ m . DMatrix @s @(Identity s) (decompose v)

-- | A module of finite-dimensional R-linear maps is finite-dimensional
instance (Commutative (Mul s), FDim s j, FDim s i) => FDim s (DMatrix s j i)
  where
  basis =
    let
    rows = dimension @s $ Proxy @i
    cols = dimension @s $ Proxy @j
    in
    matrix rows cols $ \i j -> DMatrix $ matrix rows cols $ \i' j' -> if i == i' && j == j' then one else zero
  decompose = runDMatrix

-- | The set of finite-dimensional endomorphisms on a given module forms a ring
instance FDim s a => Semigroup (Mul (DMatrix s a a))
  where
  Mul f <> Mul g = Mul $ f . g

instance FDim s a => Monoid (Mul (DMatrix s a a))
  where
  mempty = Mul $ id

instance FDim s a => Semiring (DMatrix s a a)
