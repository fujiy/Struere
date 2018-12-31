module Data.Struere.Isomorphism
    ( module Control.Isomorphism.Partial
    , module Control.Isomorphism.Partial.Prim
    , module Control.Isomorphism.Partial.TH
    , ProductFunctor(..)
    , Alter(..)
    , isomorphism
    , void
    ) where

import Control.Isomorphism.Partial
import Control.Isomorphism.Partial.Prim
import Control.Isomorphism.Partial.Unsafe (Iso(..))
import Control.Isomorphism.Partial.TH

infixl 3 <|>
infixr 6 <*>

class ProductFunctor f where
    (<*>) :: f alpha -> f beta -> f (alpha, beta)

class Alter f where
    (<|>) :: f alpha -> f alpha -> f alpha
    empty :: f alpha

isomorphism :: (alpha -> Maybe beta) -> (beta -> Maybe alpha) -> Iso alpha beta
isomorphism = Iso

void :: Iso alpha ()
void = Iso f g
  where
    f _ = Just ()
    g _ = Nothing
