--https://brianmckenna.org/blog/type_annotation_cofree
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module CofreeTree where

import Prelude hiding (sequence)

-- cabal install comonad
import Control.Comonad.Cofree
import Control.Monad.State hiding (sequence)
import Data.Foldable (Foldable, fold)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Traversable (Traversable, sequence)
import qualified Data.Map as M