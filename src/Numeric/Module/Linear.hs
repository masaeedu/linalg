module Numeric.Module.Linear where

import Numeric.Module

class (Module s v1, Module s v2, Module s l) => Hom s l v1 v2
  where
  (|$|) :: l -> v1 -> v2
  infixl 1 |$|
