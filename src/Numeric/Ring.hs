{-# LANGUAGE
    PackageImports
  , DeriveTraversable
  , DeriveGeneric
  , CPP
  , ImpredicativeTypes
#-}
module Numeric.Ring where

import Prelude

import GHC.Exts
import GHC.Generics
import qualified "base" Prelude as N (Num (..), Fractional(..))

import Data.Coerce (Coercible)
import Data.Monoid (Ap(..))
import Data.Int
import Data.Word
import Data.Functor.Identity

-- {{{ NEWTYPES

-- {{{ ADD
newtype Add a = Add { getAdd :: a }
  deriving (Foldable, Functor, Traversable, Eq, Show, Generic)
  deriving (N.Num)

(+) :: Semigroup (Add a) => a -> a -> a
i + j = getAdd $ Add i <> Add j

infixl 6 +

zero :: Monoid (Add a) => a
zero = getAdd $ mempty

-- }}}

-- {{{ MUL
newtype Mul a = Mul { getMul :: a }
  deriving (Foldable, Functor, Traversable, Eq, Show, Generic)
  deriving (N.Num)

(*) :: Semigroup (Mul a) => a -> a -> a
i * j = getMul $ Mul i <> Mul j

infixl 7 *

one :: Monoid (Mul a) => a
one = getMul $ mempty

-- }}}

-- {{{ FROMNUM

newtype FromNum a = FromNum a
  deriving (Foldable, Functor, Traversable, Eq, Show, Generic)
  deriving (N.Num, N.Fractional)

instance N.Num a => Semigroup (Add (FromNum a))
  where
  (<>) = coerce ((N.+) @a)

instance N.Num a => Semigroup (Mul (FromNum a))
  where
  (<>) = coerce ((N.*) @a)

instance N.Num a => Monoid (Add (FromNum a))
  where
  mempty = 0

instance N.Num a => Monoid (Mul (FromNum a))
  where
  mempty = 1

-- }}}

-- {{{ AP
type Parametric f = (forall a b. (Coercible a b => Coercible (f a) (f b)) :: Constraint)

type PApplicative f = (Applicative f, Parametric f)

deriving via (Ap f (Add a)) instance (Semigroup (Add a), PApplicative f) => Semigroup (Add (Ap f a))
deriving via (Ap f (Mul a)) instance (Semigroup (Mul a), PApplicative f) => Semigroup (Mul (Ap f a))
deriving via (Ap f (Add a)) instance (Monoid (Add a), PApplicative f) => Monoid (Add (Ap f a))
deriving via (Ap f (Mul a)) instance (Monoid (Mul a), PApplicative f) => Monoid (Mul (Ap f a))

-- }}}

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

instance N.Num a => Commutative (Add (FromNum a))

instance (Commutative a, PApplicative f) => Commutative (Ap f a)

deriving via (Ap f (Add a)) instance (Commutative (Add a), PApplicative f) => Commutative (Add (Ap f a))
deriving via (Ap f (Mul a)) instance (Commutative (Mul a), PApplicative f) => Commutative (Mul (Ap f a))

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

instance N.Num a => Group (Add (FromNum a))
  where
  invert = coerce (N.negate @a)

instance N.Fractional a => Group (Mul (FromNum a))
  where
  invert = coerce (N.recip @a)

instance (Group a, Applicative f) => Group (Ap f a)
  where
  invert = fmap invert

deriving via (Ap f (Add a)) instance (Group (Add a), PApplicative f) => Group (Add (Ap f a))
deriving via (Ap f (Mul a)) instance (Group (Mul a), PApplicative f) => Group (Mul (Ap f a))

-- }}}

-- {{{ SEMIRING

-- TODO: Break this up into distributivity and annihilation classes and turn this into a type synonym
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

instance N.Num a => Semiring (FromNum a)

instance (Semiring a, PApplicative f) => Semiring (Ap f a)

-- }}}

-- {{{ RING

type Ring a = (Semiring a, Group (Add a))

type CommutativeRing a = (Ring a, Commutative (Mul a))

-- }}}

-- {{{ FIELD

type Field a = (CommutativeRing a, Group (Mul a))

-- }}}

-- {{{ INSTNACES FOR TYPES FROM BASE

type Implies c d = (c => d :: Constraint)

-- {{{ TERMINAL FIELD

deriving via () instance Semigroup (Add ())
deriving via () instance Semigroup (Mul ())
deriving via () instance Monoid (Add ())
deriving via () instance Monoid (Mul ())
instance Group (Add ())
  where
  invert = id
instance Group (Mul ())
  where
  invert = id
instance Commutative (Add ())
instance Commutative (Mul ())
instance Semiring ()

-- }}}

-- {{{ MACROS
#define semigroup(t, o, v, c) \
deriving via (o v) instance Semigroup (o c) => Semigroup (o t) ;\

#define monoid(t, o, v, c) \
deriving via (o v) instance Monoid (o c) => Monoid (o t) ;\

#define group(t, o, v, c) \
deriving via (o v) instance Group (o c) => Group (o t) ;\

#define commutative(t, o, c) \
instance Commutative (o c) => Commutative (o t) ;\

#define semiring(t, v, c) \
semigroup(t, Add, v, c)           ;\
monoid(t, Add, v, c)              ;\
commutative (t, Add, c)           ;\
semigroup(t, Mul, v, c)           ;\
monoid(t, Mul, v, c)              ;\
instance Semiring c => Semiring t ;\

#define ring(t, v, c) \
semiring(t, v, c)   ;\
group(t, Add, v, c) ;\

#define cring(t, v, c) \
ring(t, v, c)          ;\
commutative(t, Mul, c) ;\

#define field(t, v, c) \
cring(t, v, c)      ;\
group(t, Mul, v, c) ;\

-- }}}

-- {{{ NUMERIC TYPES

#define ncring(t) \
cring(t, (FromNum t), ()) ;\

#define nfield(t) \
field(t, (FromNum t), ()) ;\

ncring(Int)
ncring(Int8)
ncring(Int16)
ncring(Int32)
ncring(Int64)
ncring(Integer)
ncring(Word)
ncring(Word8)
ncring(Word16)
ncring(Word32)
ncring(Word64)
nfield(Float)
nfield(Double)

-- }}}

-- {{{ VARIOUS APPLICATIVES

#define asemiring(f) \
semiring((f x), (Ap f x), x) ;\

#define aring(f) \
ring((f x), (Ap f x), x) ;\

#define afield(f) \
field((f x), (Ap f x), x) ;\

afield(Identity)
afield(((->) y))
aring(IO)
asemiring(Maybe)

-- }}}

-- }}}
