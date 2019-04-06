{-# LANGUAGE DeriveFunctor #-}

module Data.Struere.Position () where


import           Data.IntMap        (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe
import           Data.Sequence      (Seq)
import qualified Data.Sequence      as Seq
import           Prelude            hiding (null)

import           Data.Struere.Util


-- |Position

type Position = Seq Int

sub :: Int -> Position -> Position
sub = (Seq.<|)

zero :: Position
zero = Seq.singleton 0

root :: Position
root = Seq.empty

fromList :: [Int] -> Position
fromList = Seq.fromList


-- |Positioned

data Positioned a = Positioned [a] (IntMap (Positioned a))
    deriving (Eq, Show, Functor)

instance Semigroup (Positioned a) where
    Positioned lr lm <> Positioned rr rm =
        Positioned (lr <> rr) (IntMap.unionWith (<>) lm rm)

instance Monoid (Positioned a) where
    mempty = Positioned [] mempty

roots :: Positioned a -> [a]
roots (Positioned rs _) = rs

subs :: Positioned a -> IntMap (Positioned a)
subs (Positioned _ m) = m

exceptRoots :: Positioned a -> Positioned a
exceptRoots (Positioned _ m) = Positioned [] m

head :: Positioned a -> Positioned a
head (Positioned _ m) = IntMap.findWithDefault mempty 0 m

onRoot :: a -> Positioned a
onRoot a = Positioned [a] mempty

onRoots :: [a] -> Positioned a
onRoots rs = Positioned rs mempty

singleton :: Positioned a -> Positioned a
singleton = Positioned [] . IntMap.singleton 0

null :: Positioned a -> Bool
null (Positioned [] m)
    | IntMap.null m = True
null _              = False

split :: Int -> Positioned a -> (Positioned a, Positioned a)
split x (Positioned _ m) =
    let (lm, ma, rm) = IntMap.splitLookup x m
        rm'          = maybe rm (flip (IntMap.insert x) rm) ma
    in  ( Positioned [] lm
        , shift (- x) $ Positioned [] rm'
        )

merge :: Int -> Positioned a -> Positioned a -> Positioned a
merge x (Positioned lr lm) (Positioned rr rm) =
    Positioned (lr <> rr)
    $ IntMap.unionWith (<>) lm (IntMap.mapKeysMonotonic (+ x) rm)

shift :: Int -> Positioned a -> Positioned a
shift n (Positioned r m) = Positioned r $ IntMap.mapKeysMonotonic (+ n) m

listSub :: Positioned a -> [Positioned a]
listSub (Positioned _ m) =
    snd . foldl
    (\(n, ps) (i, p) ->
            ( i + 1
            , ps ++ replicate (i - n) mempty ++ [p] ))
    (0, [])
    $ IntMap.toAscList m
    -- empties from to = take (to - from)
         -- $ iterate (\(n, _) -> (n + 1, mempty)) (from, mempty)

unders :: Positioned a -> [(Int, a)]
unders (Positioned _ m) = IntMap.toAscList m >>= traverse roots

listSubInf :: Positioned a -> [Positioned a]
listSubInf = (++ repeat mempty) . listSub

positioned :: Position -> a -> Positioned a
positioned p = positioned' p . (:[])

positioned' :: Position -> [a] -> Positioned a
positioned' Seq.Empty      rs = Positioned rs mempty
positioned' (x Seq.:<| xs) rs = Positioned []
                                $ IntMap.singleton x $ positioned' xs rs


move :: Path -> Positioned a -> Positioned a
move (Path u x p) = downs p . nexts x . ups' u
  where
    ups' :: Int -> Positioned a -> Positioned a
    ups' x p = let (m, p') = ups x p
               in mconcat $ p' : IntMap.elems m

    ups :: Int -> Positioned a -> (IntMap (Positioned a), Positioned a)
    ups 0 p                 = (mempty, p)
    ups x (Positioned rs m) =
        let subt = ups x <$> m
            subm = IntMap.unionsWith (<>) . IntMap.elems $ fst <$> subt
            (mrp, subm') = IntMap.updateLookupWithKey
                           (const . const Nothing) 0 subm
            subr = IntMap.filter (not . null) $ snd <$> subt
            p' = fromMaybe mempty mrp
        in  ( IntMap.insert (x - 1) (Positioned rs mempty)
              $ IntMap.mapKeysMonotonic (subtract 1) subm'
            , p' <> Positioned [] subr
            )

    nexts :: Int -> Positioned a -> Positioned a
    nexts 0 p                 = p
    nexts x (Positioned rs m) =
        let subt = nexts x <$> m
            subr = IntMap.mapKeysMonotonic (+ x) $ roots <$> subt
            subm = subs <$> subt
            m'   = IntMap.filter (not . null)
                   $ IntMap.unionWith (<>)
                   (flip Positioned mempty <$> subr)
                   (Positioned [] <$> subm)
            -- m'' = (flip Positioned mempty <$> subr)
        in  Positioned rs m'

    downs :: Position -> Positioned a -> Positioned a
    downs Seq.Empty p         = p
    downs p (Positioned rs m) =
        let rm = positioned' p rs
            subm = downs p <$> m
        in rm <> Positioned [] subm

narrow :: Int -> Int -> Positioned a -> Positioned a
narrow minb maxb (Positioned rs m) =
    Positioned rs $
    IntMap.mapKeysWith (<>) (max minb . min maxb) m

bottom :: Positioned a -> Positioned a
bottom (Positioned rs m) =
    mconcat $ Positioned rs mempty : IntMap.elems (bottom <$> m)


-- |Path

data Path = Path Int Int Position -- ^Up, Prev/Next, Down
    deriving (Eq, Show)

instance Semigroup Path where
    Path u x Seq.Empty     <> Path 0 y q = Path u (x + y) q
    Path u x (p Seq.:|> z) <> Path 0 y q = Path u x (p Seq.>< z + y Seq.<| q)
    Path u x p             <> Path v y q | length p > v
        = Path u x (Seq.take (length p - v) p) <> Path 0 y q

instance Monoid Path where
    mempty = Path 0 0 root

next :: Int -> Path
next x = Path 0 x root

prev :: Int -> Path
prev = next . negate

up :: Int -> Path
up x | x >= 0    = Path x 0 root
     | otherwise = down $ negate x

down :: Int -> Path
down x | x >= 0    = Path 0 0 (Seq.replicate x 0)
       | otherwise = up $ negate x


