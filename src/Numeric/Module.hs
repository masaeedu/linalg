{-# LANGUAGE
    FunctionalDependencies
#-}
module Numeric.Module where

import Prelude

import Data.Proxy (Proxy(..))

import Data.Type.Nat
import Data.Vec.Pull (Vec(..))
import qualified Data.Vec.Pull as V'
import Data.Vector as V (Vector, length, zipWith)
import Data.Functor.Identity

import Numeric.Ring

{-|
A left-module over a ring

= Laws
* Compatibility of scaling with ring multiplication
@
a |*| b |*| v = a * b |*| v
@

* Identity element of scaling
@
1 |*| v = v
@

* Distributivity of scaling wrt vector addition
@
a |*| (u <> v) = a |*| u <> a |*| v
@

* Distributivity of scalar multiplication wrt ring addition
@
a + b |*| v = a |*| v <> b |*| v
@
-}
class (Ring r, Abelian (Add v)) => Module r v | v -> r
  where
  (|*|)  :: r -> v -> v
  infixr 3 |*|

-- | A vector space is simply a module over a field (instead of a mere ring)
type Space s v = (Field s, Module s v)

convertV2V :: SNatI d => Vec d x -> Vector x
convertV2V = foldMap pure

-- | A module of finite dimension, represented by a fixed set of basis vectors
class Module r v => FDim r v
  where
  basis :: Vector v
  default basis :: SDim d r v => Vector v
  basis = convertV2V sbasis

  decompose :: v -> Vector r
  default decompose :: SDim d r v => v -> Vector r
  decompose = convertV2V . sdecompose

-- | Given an array of scalars representing coordinates, build a vector
build :: FDim r v => Vector r -> v
build = getAdd . foldMap Add . V.zipWith (flip (|*|)) basis

-- | Obtain the dimension of a given finite dimensional vector space
dimension :: FDim r v => Proxy v -> Int
dimension (_ :: Proxy v) = V.length (basis :: Vector v)

-- | A module whose dimension and set of basis vectors is known at compile time
--   NB: This is an artificial distinction that is meaningless in a proper dependently-typed language
class (FDim r v, SNatI d) => SDim d r v | v -> d
  where
  sbasis :: Vec d v

  sdecompose :: v -> Vec d r

-- | Given a statically-sized array of coordinates, build a vector
sbuild :: SDim d r v => Vec d r -> v
sbuild = getAdd . foldMap Add . V'.zipWith (flip (|*|)) sbasis

-- | Obtain the dimension of a statically-dimensioned vector space as a compile time constant
sdimension :: (SDim d r v, SNatI d) => Proxy v -> SNat d
sdimension _ = snat

-- {{{ IDENTITY MODULE

instance Ring a => Module a (Identity a)
  where
  a |*| Identity v = Identity $ a * v

instance Ring a => FDim a (Identity a)

instance Ring a => SDim Nat1 a (Identity a)
  where
  sbasis = V'.singleton $ Identity $ one
  sdecompose (Identity a) = V'.singleton a

-- }}}
