{-# LANGUAGE LambdaCase, FunctionalDependencies #-}
module Numeric.VSpaces.SMatrix where

import Prelude

import Data.Type.Nat (SNatI(..), Nat(..), Mult, Plus)

import Data.Fin

import Data.Vec.Pull (Vec(..))
import qualified Data.Vec.Pull as V'

import Data.Coerce (coerce)

import Control.Category (Category(..))
import Control.Applicative (liftA2)

import Numeric.Ring
import Numeric.Module
import Numeric.VSpaces.DMatrix

-- {{{ N-DIMENSIONAL VECTORS

newtype V n a = V { runV :: Vec n a }
  deriving (Functor, Applicative, Monad)

instance (Show a, SNatI n) => Show (V n a)
  where
  show (V v) = show $ convertV2V v

deriving via (Vec n (Add a)) instance Semigroup (Add a) => Semigroup (Add (V n a))
deriving via (Vec n (Add a)) instance Monoid    (Add a) => Monoid    (Add (V n a))

instance Commutative (Add a) => Commutative (Add (V n a))

instance Group (Add a) => Group (Add (V n a))
  where
  invert (Add (V (Vec f))) = Add $ V $ Vec $ getAdd . invert . Add . f

instance Ring a => Module a (V n a)
  where
  s |*| V (Vec v) = V $ Vec $ (s *) . v

instance
  ( Ring a, SNatI n
  , SDim n a (V n a)
  ) => FDim a (V n a)
  where
  decompose (V v) = convertV2V v

instance Ring a => SDim 'Z a (V 'Z a)
  where
  sbasis = Vec $ const $ V $ V'.empty
  sdecompose = runV

instance
  ( SNatI n
  , Ring a
  , SDim n a (V n a)
  ) => SDim ('S n) a (V ('S n) a)
  where
  sbasis = Vec $ V . \case
    { FZ -> V'.cons one (Vec $ const $ zero)
    ; FS s -> V'.cons zero $ runV $ (unVec $ sbasis) s
    }
  sdecompose = runV

-- }}}

-- {{{ LINEAR MAPS: STATIC DIMENSION

-- | An @SMatrix s b a@ is a matrix of @a@ rows and @b@ columns. The somewhat awkward ordering of the type parameters
--   arises from the way we wish the category instance and matrix multiplication to work (the "row, column" order more
--   natural in plain speaking would give us the dual of the category we want).
data SMatrix s j i = (SNatI j => SNatI i) => SMatrix { runSMatrix :: SNatI j => V'.Vec i (V'.Vec j s) }

toDMatrix :: (SDim i s (V i s), SDim j s (V j s)) => SMatrix s i j -> DMatrix s (V i s) (V j s)
toDMatrix (SMatrix v) = DMatrix $ V'.foldMap convertV2V v

indexSM :: SNatI j => Fin i -> Fin j -> SMatrix s j i -> s
indexSM i j (SMatrix v) = unVec (unVec v i) j

scol :: SNatI j => Fin j -> SMatrix s j i -> Vec i s
scol j (SMatrix v) = fmap (($ j) . unVec) v

srow :: SNatI j => Fin i -> SMatrix s j i -> Vec j s
srow i (SMatrix v) = unVec v i

instance (Show s, SDim i s (V i s), SDim j s (V j s)) => Show (SMatrix s j i)
  where
  show x = show $ convertV2V $ fmap convertV2V $ runSMatrix x

instance Ring s => Category (SMatrix s)
  where
  id = SMatrix $ Vec $ \i -> Vec $ \j -> if i == j then one else zero
  SMatrix v . SMatrix w = SMatrix $ Vec $ \i -> Vec $ \j -> getAdd $ foldMap (Add . liftA2 (*) (coerce v i) (fmap ($ j) $ coerce w)) universe

-- Equip with abelian group structure
instance Ring s => Semigroup (Add (SMatrix s j i))
  where
  Add (SMatrix v) <> Add (SMatrix w) = Add $ SMatrix $ liftA2 (liftA2 (+)) v w

instance Ring s => Commutative (Add (SMatrix s j i))

instance (Ring s, SNatI i) => Monoid (Add (SMatrix s j i))
  where
  mempty = Add $ SMatrix $ pure $ pure $ zero

instance (Ring s, SNatI i) => Group (Add (SMatrix s j i))
  where
  invert (Add (SMatrix v)) = Add $ SMatrix $ fmap (fmap negate) v

-- Equip with module structure
instance (CommutativeRing s, SNatI i) => Module s (SMatrix s j i)
  where
  s |*| SMatrix v = SMatrix $ fmap (fmap (s *)) v

instance (CommutativeRing s, SNatI j, SNatI i, SNatI (Mult i j), Flatten i j (Mult i j)) => FDim s (SMatrix s j i)

-- {{{ TYPE LEVEL FINAGLING FOR STATICALLY SIZED VECTORS

class Mult a b ~  c => Flatten a b c | a b -> c
  where
  flatten :: Vec a (Vec b x) -> Vec c x

instance Flatten 'Z x 'Z
  where
  flatten = const V'.empty

instance (Flatten n m r', Plus m r' ~ r, Append m r' r) => Flatten ('S n) m r
  where
  flatten v = vappend (V'.head v) (flatten $ V'.tail v)

class Plus a b ~ c => Append a b c | a b -> c
  where
  vappend :: Vec a x -> Vec b x -> Vec c x

instance Append 'Z x x
  where
  vappend = flip const

instance Append n m r => Append ('S n) m ('S r)
  where
  vappend x y = V'.cons (V'.head x) (vappend (V'.tail x) y)

-- }}}

instance (CommutativeRing s, SNatI j, SNatI i, SNatI c, Mult i j ~ c, Flatten i j c) => SDim c s (SMatrix s j i)
  where
  sbasis = flatten $ Vec $ \i -> Vec $ \j -> SMatrix $ Vec $ \i' -> Vec $ \j' -> if i == i' && j == j' then one else zero
  sdecompose (SMatrix v) = flatten v

instance (CommutativeRing s, SDim j s x, SDim i s y) => Hom s (SMatrix s j i) x y
  where
  m@(SMatrix _) |$| v = sbuild $ fmap V'.head $ runSMatrix $ (m .) $ SMatrix $ fmap V'.singleton $ sdecompose v

-- The endomorphism group of matrices on a given vector space is a full ring
instance Ring s => Semigroup (Mul (SMatrix s a a))
  where
  (<>) = coerce ((.) @(SMatrix s))

instance Ring s => Monoid (Mul (SMatrix s a a))
  where
  mempty = coerce (id @(SMatrix s))

instance (Ring s, SNatI a) => Semiring (SMatrix s a a)

-- }}}
