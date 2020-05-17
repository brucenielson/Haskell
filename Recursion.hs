-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html
-- https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/syntax-extns.html

-- https://jtobin.io/time-traveling-recursion

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Comonad.Cofree
import Control.Monad.Free
import Data.Functor.Foldable


oddIndices :: [a] -> [a]
oddIndices = histo $ \case
    Nil                           -> []
    Cons h (_ :< Nil)             -> [h]
    Cons h (_ :< Cons _ (t :< _)) -> h:t:l 

