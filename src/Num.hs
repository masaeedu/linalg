{-# LANGUAGE
    DeriveTraversable
#-}
module Num where

import Prelude hiding (Num(..), recip, id, (.))
import qualified Prelude as N (Num (..))

import Control.Category (Category(..))

-- {{{ NEWTYPES

newtype Add a = Add { getAdd :: a }
  deriving (Foldable, Functor, Traversable)
  deriving (N.Num)

(+) :: Semigroup (Add a) => a -> a -> a
i + j = getAdd $ Add i <> Add j

infixl 6 +

zero :: Monoid (Add a) => a
zero = getAdd $ mempty

newtype Mul a = Mul { getMul :: a }
  deriving (Foldable, Functor, Traversable)
  deriving (N.Num)

(*) :: Semigroup (Mul a) => a -> a -> a
i * j = getMul $ Mul i <> Mul j

infixl 7 *

one :: Monoid (Mul a) => a
one = getMul $ mempty

newtype FromNum a = FromNum a
  deriving N.Num

instance N.Num a => Semigroup (Add (FromNum a))
  where
  i <> j = i N.+ j

instance N.Num a => Semigroup (Mul (FromNum a))
  where
  i <> j = i N.* j

instance N.Num a => Monoid (Add (FromNum a))
  where
  mempty = 0

instance N.Num a => Monoid (Mul (FromNum a))
  where
  mempty = 1

deriving via (Add (FromNum Integer)) instance Semigroup (Add Integer)
deriving via (Add (FromNum Integer)) instance Monoid    (Add Integer)

deriving via (Mul (FromNum Integer)) instance Semigroup (Mul Integer)
deriving via (Mul (FromNum Integer)) instance Monoid    (Mul Integer)

deriving via (Add (FromNum Int))     instance Semigroup (Add Int)
deriving via (Add (FromNum Int))     instance Monoid    (Add Int)

deriving via (Mul (FromNum Int))     instance Semigroup (Mul Int)
deriving via (Mul (FromNum Int))     instance Monoid    (Mul Int)

-- }}}

-- {{{ COMMUTATIVE

{-|
A semigroup with commutative multiplication

= Laws
* Commutativity
@
  u <> v = v <> u
@
-}
class Semigroup g => Commutative g

instance Commutative (Add Int)
instance Commutative (Mul Int)
instance Commutative (Add Integer)
instance Commutative (Mul Integer)

-- }}}

-- {{{ GROUP

{-|
A group is a monoid where every element has an inverse

= Laws
* Inverse
@
  v <> invert v = mempty
@
-}
class Monoid g => Group g
  where
  invert :: g -> g

type Abelian g = (Commutative g, Group g)

negate :: Group (Add a) => a -> a
negate = getAdd . invert . Add

(-) :: Group (Add a) => a -> a -> a
a - b = a + negate b

reciprocal :: Group (Mul a) => a -> a
reciprocal = getMul . invert . Mul

(/) :: Group (Mul a) => a -> a -> a
a / b = a * reciprocal b

instance Group (Add Int)
  where
  invert (Add i) = Add $ -i

-- }}}

-- {{{ SEMIRING

{-|
Two monoids structures on the same set: an "additive" one which is commutative, and a "multiplicative" one over which the additive structure distributes

= Laws
* Left distributivity
@
  x * (y + z) = (x * y) + (x * z)
@

* Right distributivity
@
  (x + y) * z = (x * z) + (y * z)
@

* Annihilation
@
  0 * x = 0
@
-}
class (Commutative (Add a), Monoid (Add a), Monoid (Mul a)) => Semiring a

instance Semiring Int
instance Semiring Integer

-- }}}

-- {{{ RING

type Ring a = (Semiring a, Group (Add a))
type CommutativeRing a = (Ring a, Commutative (Mul a))

-- }}}

-- {{{ FIELD

type Field a = (Ring a, Group (Mul a))

-- }}}

