{-# LANGUAGE PackageImports #-}
module Prelude (module P, module C) where

import "base" Prelude as P hiding (id, (.), Num(..), Fractional(..))
import Control.Category as C (Category(..))
