{-# LANGUAGE
    FunctionalDependencies
  , DeriveTraversable
  , PartialTypeSignatures
  , InstanceSigs
  , LambdaCase
  , ViewPatterns
  , PatternSynonyms
#-}
module LinearAlgebra where

import Prelude hiding (Num(..), recip, id, (.))
import qualified Prelude as N (Num (..))

import Data.Proxy (Proxy (..))
import Data.Type.Nat (SNatI(..), SNat(..), Nat(..), Nat0, Nat1, Nat2, Nat3, Mult, Plus)
import Data.Foldable (fold)

import Data.Vector (Vector)
import qualified Data.Vector as V (length, generate, slice, (!), replicate)

import Data.Fin

import Data.Vec.Pull (Vec(..))
import qualified Data.Vec.Pull as V'

import Data.Void
import Data.Coerce

import Control.Category (Category(..))
import Control.Applicative (liftA2)
import Control.Monad (join)

-- {{{ CLASSES

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

-- {{{ MODULES

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
class (Ring r, Abelian v) => Module r v | v -> r
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
build = fold . liftA2 (flip (|*|)) basis

-- | Obtain the dimension of a given finite dimensional vector space
dimension :: FDim r v => Proxy v -> Int
dimension (_ :: Proxy v) = V.length (basis :: Vector v)

-- | A module whose dimension and set of basis vectors is known at compile time
--   NB: This is an artificial distinction that is meaningless in a proper dependently-typed language
class (FDim r v, SNatI d) => SDim d r v | v -> d
  where
  sbasis :: Vec d v

  sdecompose :: v -> Vec d r

-- | Given a statically-sized vector of coordinates, build a vector
sbuild :: SDim d r v => Vec d r -> v
sbuild = fold . liftA2 (flip (|*|)) sbasis

-- | Obtain the dimension of a statically-dimensioned vector space as a compile time constant
sdimension :: (SDim d r v, SNatI d) => Proxy v -> SNat d
sdimension _ = snat

-- }}}

-- {{{ LINEAR MAPS

class (Module s v1, Module s v2, Module s l) => Hom s l v1 v2
  where
  (|$|) :: l -> v1 -> v2
  infixl 1 |$|

-- }}}

-- }}}

-- {{{ LINEAR MAPS: ARBITRARY DIMENSION

{-|
A module homomorphism between two ring-modules

= Laws
* Preservation of addition
@
f |$| v <> w = (f |$| v) <> (f |$| w)
@

* Preservation of scaling
@
f |$| a |*| v = a |*| (f |$| v)
@
-}
data Linear s a b = (Module s a => Module s b) => Linear (Module s a => a -> b)

-- | Linear maps and vector spaces form a category
instance Category (Linear s)
  where
  id = Linear id
  Linear f . Linear g = Linear $ f . g

-- | Linear maps inherit a pointwise abelian group structure from their codomain
instance Module s b => Semigroup (Linear s a b)
  where
  Linear x <> Linear y = Linear $ \a -> x a <> y a

instance Module s b => Commutative (Linear s a b)

instance Module s b => Monoid (Linear s a b)
  where
  mempty = Linear $ const mempty

instance Module s b => Group (Linear s a b)
  where
  invert (Linear f) = Linear $ invert . f

-- | The abelian group of R-linear maps is a full module when R is a commutative ring
instance (Commutative (Mul s), Module s b) => Module s (Linear s a b)
  where
  s |*| Linear f = Linear $ (s |*|) . f

instance (Commutative (Mul s), Module s a, Module s b) => Hom s (Linear s a b) a b
  where
  Linear f |$| a = f a

-- | The set of endomorphisms on a given module forms a ring
deriving instance Module s a => Semigroup   (Add (Linear s a a))
deriving instance Module s a => Commutative (Add (Linear s a a))
deriving instance Module s a => Monoid      (Add (Linear s a a))
deriving instance Module s a => Group       (Add (Linear s a a))

instance Module s a => Semigroup (Mul (Linear s a a))
  where
  Mul f <> Mul g = Mul $ f . g

instance Module s a => Monoid (Mul (Linear s a a))
  where
  mempty = Mul $ id

instance Module s a => Semiring (Linear s a a)

-- }}}

-- {{{ LINEAR MAPS: FINITE DIMENSION

-- | A @DMatrix s b a@ is a matrix of @a@ rows and @b@ columns. The somewhat awkward ordering of the type parameters
--   arises from the way we wish the category instance and matrix multiplication to work (the "row, column" order more
--   natural in plain speaking would give us the dual of the category we want).
data DMatrix s j i = (FDim s i => FDim s j) => DMatrix { runDMatrix :: FDim s i => Vector s }

deriving instance (Show s, FDim s i) => Show (DMatrix s j i)

-- {{{ VECTOR FINAGLING

-- | Given a number of rows and a number of columns, generates an array representing a matrix of given dimension
matrix :: Int -> Int -> (Int -> Int -> x) -> Vector x
matrix n m f = join $ V.generate n $ \i -> V.generate m $ \j -> f i j

-- | Retrieves a row from a matrix of specified dimension
row :: Int -> Int -> Int -> Vector x -> Vector x
row n m i = V.slice (i * n) m

-- | Retrieves a column from a matrix of specified dimension
col :: Int -> Int -> Int -> Vector x -> Vector x
col n m j v = V.generate n $ \i -> v V.! ((i * m) + j)

-- | Generates a matrix of specified dimension containing a given value at a given cell
cell :: Monoid (Add x) => Int -> Int -> Int -> Int -> x -> Vector x
cell n m i j v = matrix n m $ \i' j' -> if i == i' && j == j' then v else zero

-- | Produces the dot product of two vectors
dotproduct :: (Monoid (Add r), Monoid (Mul r)) => Vector r -> Vector r -> r
dotproduct x y = getAdd $ foldMap Add $ liftA2 (*) x y

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
instance FDim s j => Semigroup (DMatrix s j i)
  where
  DMatrix a <> DMatrix b = DMatrix $ liftA2 (+) a b

instance FDim s j => Commutative (DMatrix s j i)

instance (FDim s i, FDim s j) => Monoid (DMatrix s j i)
  where
  mempty = DMatrix $ V.replicate (dimension (Proxy @i) * dimension (Proxy @j)) zero

instance (FDim s i, FDim s j) => Group (DMatrix s j i)
  where
  invert (DMatrix v) = DMatrix $ fmap negate v

-- | The abelian group of finite-dimensional R-linear maps is a full module when R is a commutative ring
instance (Commutative (Mul s), FDim s i, FDim s j) => Module s (DMatrix s j i)
  where
  s |*| DMatrix v = DMatrix $ fmap (s *) v

instance (Commutative (Mul s), FDim s i, FDim s j) => Hom s (DMatrix s j i) j i
  where
  m |$| v = build $ runDMatrix $ m . DMatrix @s @(V Nat1 s) (decompose v)

-- | A module of finite-dimensional R-linear maps is finite-dimensional
instance (Commutative (Mul s), FDim s a, FDim s b) => FDim s (DMatrix s a b)
  where
  basis =
    let
    n = dimension @s $ Proxy @a
    m = dimension @s $ Proxy @b
    in
    matrix n m $ \i j -> DMatrix $ matrix n m $ \i' j' -> if i == i' && j == j' then one else zero
  decompose = runDMatrix

-- | The set of finite-dimensional endomorphisms on a given module forms a ring
deriving instance FDim s a => Semigroup   (Add (DMatrix s a a))
deriving instance FDim s a => Commutative (Add (DMatrix s a a))
deriving instance FDim s a => Monoid      (Add (DMatrix s a a))
deriving instance FDim s a => Group       (Add (DMatrix s a a))

instance FDim s a => Semigroup (Mul (DMatrix s a a))
  where
  Mul f <> Mul g = Mul $ f . g

instance FDim s a => Monoid (Mul (DMatrix s a a))
  where
  mempty = Mul $ id

instance FDim s a => Semiring (DMatrix s a a)

-- }}}

-- {{{ N-DIMENSIONAL VECTORS

newtype V n a = V { runV :: Vec n a }
  deriving (Functor, Applicative, Monad)

instance (Show a, SNatI n) => Show (V n a)
  where
  show (V v) = show $ convertV2V v

class Construct n v c | n v -> c
  where
  vec :: c -> V'.Vec n v

instance Construct Nat0 Void ()
  where
  vec = const V'.empty

instance Construct Nat1 a a
  where
  vec = V'.singleton

instance Construct Nat2 v (v, v)
  where
  vec (a, b) = V'.cons a $ V'.cons b $ V'.empty

instance Construct Nat3 v (v, v, v)
  where
  vec (a, b, c) = V'.cons a $ V'.cons b $ V'.cons c $ V'.empty

vector :: Construct n v c => c -> V n v
vector = V . vec

deriving via (Vec n (Add a)) instance Semigroup (Add a) => Semigroup (V n a)
deriving via (Vec n (Add a)) instance Monoid    (Add a) => Monoid    (V n a)

instance Commutative (Add a) => Commutative (V n a)

instance Group (Add a) => Group (V n a)
  where
  invert (V (Vec f)) = V $ Vec $ getAdd . invert . Add . f

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

data SMatrix s j i = (SNatI i => SNatI j) => SMatrix { runSMatrix :: SNatI i => V'.Vec i (V'.Vec j s) }

toDMatrix :: (SDim i s (V i s), SDim j s (V j s)) => SMatrix s i j -> DMatrix s (V i s) (V j s)
toDMatrix (SMatrix v) = DMatrix $ V'.foldMap convertV2V v

indexSM :: SNatI i => Fin i -> Fin j -> SMatrix s j i -> s
indexSM i j (SMatrix v) = unVec (unVec v i) j

scol :: SNatI i => Fin j -> SMatrix s j i -> Vec i s
scol j (SMatrix v) = fmap (($ j) . unVec) v

srow :: SNatI i => Fin i -> SMatrix s j i -> Vec j s
srow i (SMatrix v) = unVec v i

instance (Show s, Ring s, SDim i s (V i s), SDim j s (V j s)) => Show (SMatrix s j i)
  where
  show x = show $ convertV2V $ fmap convertV2V $ runSMatrix x

instance Ring s => Category (SMatrix s)
  where
  id = SMatrix $ Vec $ \i -> Vec $ \j -> if i == j then one else zero
  SMatrix v . SMatrix w = SMatrix $ Vec $ \i -> Vec $ \j -> getAdd $ foldMap (Add . liftA2 (*) (coerce v i) (fmap ($ j) $ coerce w)) universe

-- Equip with abelian group structure
instance Ring s => Semigroup (SMatrix s j i)
  where
  SMatrix v <> SMatrix w = SMatrix $ liftA2 (liftA2 (+)) v w

instance Ring s => Commutative (SMatrix s j i)

instance (Ring s, SNatI j) => Monoid (SMatrix s j i)
  where
  mempty = SMatrix $ pure $ pure $ zero

instance (Ring s, SNatI j) => Group (SMatrix s j i)
  where
  invert (SMatrix v) = SMatrix $ fmap (fmap negate) v

-- Equip with module structure
instance (CommutativeRing s, SNatI j) => Module s (SMatrix s j i)
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

instance (CommutativeRing s, SDim a s x, SDim b s y) => Hom s (SMatrix s a b) x y
  where
  m@(SMatrix _) |$| v = sbuild $ fmap V'.head $ runSMatrix $ (m .) $ SMatrix $ fmap V'.singleton $ sdecompose v

-- }}}

-- {{{ TESTING

v1 :: V Nat2 Int
v1 = vector (1, 0)

m1 :: SMatrix Int Nat2 Nat2
m1 = SMatrix $ vec (vec (0, -1), vec (1, 0))

m2 :: SMatrix Int Nat2 Nat2
m2 = SMatrix $ vec (vec (1, 2), vec (3, 4))

m3 :: SMatrix Int Nat2 Nat2
m3 = SMatrix $ vec (vec (5, 6), vec (7, 8))

result :: V Nat2 Int
result = m1 . m2 . m3 |$| v1

-- }}}