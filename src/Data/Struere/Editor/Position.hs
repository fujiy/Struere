{-# LANGUAGE DeriveFunctor #-}

module Data.Struere.Editor.Position where


import           Data.IntMap              (IntMap)
import qualified Data.IntMap.Strict       as IntMap

import           Data.Struere.Editor.Util


-- Position

type Position = [Int]

sub :: Int -> Position -> Position
sub = (:)

zero :: Position
zero = [0]

rootP :: Position
rootP = []


-- Positioned

data Positioned a = Positioned [a] (IntMap (Positioned a))
    deriving (Eq, Show, Functor)

instance Semigroup (Positioned a) where
    Positioned lr lm <> Positioned rr rm =
        Positioned (lr <> rr) (IntMap.unionWith (<>) lm rm)

instance Monoid (Positioned a) where
    mempty = Positioned [] mempty

roots :: Positioned a -> [a]
roots (Positioned rs _) = rs

head :: Positioned a -> Positioned a
head (Positioned _ m) = IntMap.findWithDefault mempty 0 m

root :: a -> Positioned a
root a = Positioned [a] mempty

singleton :: Positioned a -> Positioned a
singleton = Positioned [] . IntMap.singleton 0

null :: Positioned a -> Bool
null (Positioned [] m)
    | IntMap.null m = True
null _              = False

split :: Nat -> Positioned a -> (Positioned a, Positioned a)
split n (Positioned _ m) =
    let (lm, rm) = IntMap.split (fromNat n) m
    in  ( Positioned [] lm
        , shift (- fromNat n) $ Positioned [] rm
        )

merge :: Nat -> Positioned a -> Positioned a -> Positioned a
merge n (Positioned lr lm) (Positioned rr rm) =
    Positioned (lr <> rr)
    $ IntMap.union lm (IntMap.mapKeys (+ fromNat n) rm)

shift :: Int -> Positioned a -> Positioned a
shift n (Positioned r m) = Positioned r $ IntMap.mapKeys (+ n) m

positioned :: Position -> a -> Positioned a
positioned []     a = root a
positioned (x:xs) a = Positioned [] $ IntMap.singleton x $ positioned xs a
