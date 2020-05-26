{-# LANGUAGE FunctionalDependencies #-}
module Scratch where

import Prelude
import Data.Type.Nat
import Data.Vec.Pull as V'
import Data.Void

import Numeric.Module.Linear
import Numeric.Module.Linear.SMatrix

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
