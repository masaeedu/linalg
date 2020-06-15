{-# LANGUAGE FunctionalDependencies, BangPatterns #-}
module Main where

import Prelude

import Data.Fin
import Data.Type.Nat
import Data.Vec.Pull as V
import Data.Void
import Data.Semigroup

import System.CPUTime
import Data.Time.Clock

import Numeric
import Numeric.VSpaces.DMatrix
import Numeric.VSpaces.SMatrix

-- Vector construction
class Construct n v c | n v -> c
  where
  vec :: c -> Vec n v
  unvec :: (c -> r) -> Vec n v -> r

instance Construct Nat0 Void ()
  where
  vec = const V.empty
  unvec f = const $ f ()

instance Construct Nat1 a a
  where
  vec = V.singleton
  unvec f = f . V.head

instance Construct Nat2 v (v, v)
  where
  vec (a, b) = V.cons a $ V.cons b $ V.empty
  unvec f v = f (unVec v FZ, unVec v (FS FZ))

instance Construct Nat3 v (v, v, v)
  where
  vec (a, b, c) = V.cons a $ V.cons b $ V.cons c $ V.empty
  unvec f v = f (unVec v FZ, unVec v (FS FZ), unVec v (FS (FS (FZ))))

vector :: Construct n v c => c -> V n v
vector = V . vec

unvector :: Construct n v c => (c -> r) -> V n v -> r
unvector f = unvec f . runV

-- Testing
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

-- Fibonacci series
step :: SMatrix Integer Nat2 Nat2
step = SMatrix $ vec (vec (0, 1), vec (1, 1))

dstep :: DMatrix Integer (V Nat2 Integer) (V Nat2 Integer)
dstep = toDMatrix step

dpower :: Integer -> DMatrix Integer (V Nat2 Integer) (V Nat2 Integer)
dpower n = getMul $ mtimesDefault n $ Mul dstep

nthFib :: Integer -> Integer
nthFib n = unvector fst $ (dpower n |$| vector @Nat2 (0, 1) :: V Nat2 Integer)

main :: IO ()
main = do
  start <- getCPUTime
  let !_ = nthFib $ 10^7
  end <- getCPUTime
  print $ picosecondsToDiffTime $ end - start
