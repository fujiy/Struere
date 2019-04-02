
module Data.Struere.Isomorphism
    ( module Control.Isomorphism.Partial
    , module Control.Isomorphism.Partial.Derived
    , module Control.Isomorphism.Partial.Prim
    , module Control.Isomorphism.Partial.TH
    , isomorphism
    , (>$<)
    , first, second, (&&&)
    , foldl1, pap, pap'
    ) where

import           Control.Category
import           Control.Isomorphism.Partial
import           Control.Isomorphism.Partial.Derived
import           Control.Isomorphism.Partial.Prim
import           Control.Isomorphism.Partial.TH
import           Control.Isomorphism.Partial.Unsafe  (Iso (..))
import           Prelude                             hiding (foldl, foldl1, id,
                                                      (.), (<$>), (<*>))


-- |Basic functions

isomorphism :: (alpha -> Maybe beta) -> (beta -> Maybe alpha) -> Iso alpha beta
isomorphism = Iso

infixl 4 >$<
(>$<) :: IsoFunctor f => Iso alpha beta -> f beta -> f alpha
iso >$< fb = inverse iso <$> fb

-- void :: Iso alpha ()
-- void = Iso f g
--   where
--     f _ = Just ()
--     g _ = Nothing

first :: Iso alpha beta -> Iso (alpha, gamma) (beta, gamma)
first = (*** id)

second :: Iso alpha beta -> Iso (gamma, alpha) (gamma, beta)
second = (id ***)

infixr 3 &&&
(&&&) :: Eq alpha =>
         Iso alpha beta -> Iso alpha beta' -> Iso alpha (beta, beta')
f &&& g = (f *** g)
    . Iso (\a -> Just (a, a))
          (\(a, a') -> if a == a' then Just a else Nothing)

-- infixr 2 +++
-- (+++) :: Iso alpha beta -> Iso alpha' beta'
--       -> Iso (Either alpha alpha') (Either beta beta')
-- f +++ g = _

foldl1 :: Iso (alpha, alpha) alpha -> Iso [alpha] alpha
foldl1 f = foldl f . inverse cons

pap :: Eq alpha => Iso (alpha, beta) gamma -> alpha -> Iso beta gamma
pap iso a = (iso . (element a *** id)) . commute . unit

pap' :: Iso (alpha, beta) gamma -> alpha -> Iso beta gamma
pap' iso a = (iso . (inverse (ignore a) *** id)) . commute . unit
