
module Data.Struere.Editor.Util where

-- Nat

data Nat = Zero | Succ Nat
    deriving (Eq, Ord)

instance Num Nat where
    (+) n  = foldNat n Succ
    (*) n  = foldNat 0 (+ n)
    negate = undefined
    abs    = id
    signum = const 1
    fromInteger 0 = Zero
    fromInteger x | x > 0 = Succ $ fromInteger $ x - 1

nat :: r -> (Nat -> r) -> Nat -> r
nat z s Zero     = z
nat z s (Succ n) = s n

foldNat :: r -> (r -> r) -> Nat -> r
foldNat z s = nat z (s . foldNat z s)

fromNat :: Num n => Nat -> n
fromNat Zero     = 0
fromNat (Succ n) = fromNat n + 1

natLT :: Nat -> Nat -> Bool
natLT Zero     (Succ _) = True
natLT (Succ n) (Succ m) = natLT n m
natLT _        _        = False



-- Utility functions

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True  f a = f a
applyWhen False _ a = a
