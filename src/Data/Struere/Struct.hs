{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

module Data.Struere.Struct where


import           Control.Applicative        (Const (..))
import qualified Control.Applicative        as Ap

import qualified Control.Arrow              as Arrow
import           Control.Category
import           Control.Monad
import           Control.Monad.State.Strict
-- import           Control.Monad.Identity
-- import           Data.Coerce
import           Data.IntMap                (IntMap)
import qualified Data.IntMap                as IntMap
import qualified Data.IntSet                as IntSet
import           Data.Maybe
import qualified Data.Sequence              as Seq
import           Prelude                    hiding (id, product, pure, (.),
                                             (<$>), (<*>))
import           System.Mem.StableName
import           Type.Reflection            (TypeRep, Typeable, typeOf)
import           Unsafe.Coerce
-- import           Prelude                  as P

import           Data.Struere.Isomorphism   hiding (cons)
import           Data.Struere.Syntax        hiding (not)
import           Data.Struere.Util          hiding (Nat (..))

import           Debug.Trace


-- |Position

type Position = Seq.Seq Int

sub :: Int -> Position -> Position
sub = (Seq.<|)

zero :: Position
zero = Seq.singleton 0

root :: Position
root = Seq.empty

fromList :: [Int] -> Position
fromList = Seq.fromList


-- data Distr d = None
--              | Pure d
--              | DCons (Distr d) (Distr d)
--              | DSub  (Distr d)

-- |Distr

data Distr a = Distr a (Child a)
    deriving (Eq, Show, Functor, Foldable)

data Child a = Leaf
             | Cons (Distr a) (Distr a)
             | Sub  (Distr a)
    deriving (Eq, Show, Functor, Foldable)

-- type Diff a = Distr (Maybe a)

instance Semigroup a => Semigroup (Distr a) where
    Distr x cx <> Distr y cy = Distr (x <> y) (cx <> cy)

instance Monoid a => Monoid (Distr a) where
    mempty = Distr mempty mempty

instance Semigroup a => Semigroup (Child a) where
    Leaf     <> x        = x
    x        <> Leaf     = x
    Cons x y <> Cons z w = Cons (x <> z) (y <> w)
    Sub x    <> Sub y    = Sub (x <> y)
    -- x        <> _        = x

instance Semigroup a => Monoid (Child a) where
    mempty = Leaf

deprod :: Monoid a => Child a -> (Distr a, Distr a)
deprod (Cons x y) = (x, y)
deprod _          = (mempty, mempty)

desub :: Monoid a => Child a -> Distr a
desub (Sub x) = x
desub _       = mempty

fromPosition :: Monoid a => Struct -> Position -> a -> Distr a
fromPosition _            Seq.Empty            a = Distr a Leaf
fromPosition (Distr _ dc) pos@(x Seq.:<| pos') a = case dc of
    Leaf       -> Distr a Leaf
    Cons dx dy | x < size dx
               -> let d = fromPosition dx pos a
                  in  Distr mempty (Cons d mempty)
    Cons dx dy -> let d = fromPosition dy (x - size dx Seq.<| pos') a
                  in  Distr mempty (Cons mempty d)
    Sub dx     -> if Seq.null pos'
                  then Distr a Leaf
                  else let d = fromPosition dx pos' a
                       in  Distr mempty (Sub d)

flatten :: Monoid a => Distr a -> Distr a
flatten d = Distr (mconcatF d) mempty

flatten' :: Monoid a => Distr a -> a
flatten' = mconcatF

-- newtype Distributor r d a = Distributor
--     { distribute :: forall f g. (Syntax f, Syntax g)
--                  => (forall x. f x -> r d -> g x) -> f a -> Distr r d -> g a }


-- data Diff a = Diff [a] (Distr (Diff a))

-- instance Semigroup (Diff a) where
--     Diff xs dx <> Diff ys dy = Diff (xs <> ys) (dx <> dy)

-- instance Monoid (Diff a) where
--     mempty = Diff [] mempty


-- |Struct

type Struct = Distr Int

size :: Struct -> Int
size (Distr x _) = x

unit :: Struct
unit = Distr 1 Leaf

-- sub :: Struct -> Struct
-- sub = Struct 1 True . Sub


-- data Struct = Struct
--     { size      :: Int
--     , isSingle  :: Bool
--     , substract :: Substract
--     }

-- instance Show Struct where
--     show (Struct x s sub) =
--         unwords [ show x
--                 , if s then "s" else "m"
--                 , "(" ++ show sub ++ ")"]


-- data Substract = Unit
--                | Sub Struct
--                | Cons Struct Struct
--     deriving Show

newtype Radiographer a = Radiographer
    { xray :: a -> Maybe Struct }

instance IsoFunctor Radiographer where
    iso <$> Radiographer f =
        Radiographer $ unapply iso >=> f

instance ProductFunctor Radiographer where
    Radiographer f <*> Radiographer g =
        Radiographer $ \(a, b) -> do
            sta <- f a
            stb <- g b
            return . Distr (size sta + size stb) $ Cons sta stb

instance Alter Radiographer where
    empty = Radiographer $ \_ -> return $ Distr 0 Leaf
    Radiographer f <|> Radiographer g =
        Radiographer $ \a -> f a `mplus` g a

instance Syntax Radiographer where
    pure _ = Radiographer $ \_ -> return $ Distr 0 Leaf
    char   = Radiographer $ \_ -> return $ Distr 1 Leaf
    part d (Radiographer f) =
        Radiographer $ \a -> do
            ac <- f a
            let size = if isOrnament d then 0 else 1
            return $ Distr size (Sub ac)

-- |Caret

type Carets = Distr Caret

data Caret = NoCaret
           | InsertCaret
           | ReplaceCaret
    deriving (Eq, Show)

instance Semigroup Caret where
    NoCaret <> x = x
    x       <> _ = x

instance Monoid Caret where
    mempty = NoCaret

data CaretMove = CaretUp
               | CaretDown
               | CaretNext
               | CaretPrev

move :: Monoid a => CaretMove -> Struct -> Distr a -> Distr a
move cm st d = case cm of
    -- CaretStay -> rail'
    CaretUp   -> uncurry (narrowFirst st) $ up st d
        -- let (Distr a dc, a') = up st d
        --          in  Distr (a <> a') dc
    CaretDown -> down st d mempty
    CaretNext -> uncurry (narrowLast  st) $ next st True d mempty
    CaretPrev -> uncurry (narrowFirst st) $ prev st d mempty
  where
    up :: Monoid a => Struct -> Distr a -> (Distr a, a)
    up (Distr _ sc) d@(Distr a dc) = case sc of
        Leaf       -> (mempty, flatten' d)
        Cons sx sy ->
            let (dx, dy) = deprod dc
                (dx', b) = up sx dx
                (dy', c) = up sy dy
            in  ( Distr mempty (Cons dx' dy'), a <> b <> c )
        Sub sx     ->
            let (dx', b) = up sx (desub dc)
            in  ( Distr b (Sub dx'), a )

    down :: Monoid a => Struct -> Distr a -> a -> Distr a
    down (Distr _ sc) (Distr a dc) a' = case sc of
        Leaf       -> Distr (a <> a') dc
        Cons sx sy ->
            let (dx, dy) = deprod dc
                dx'      = down sx dx (a <> a')
                dy'      = down sy dy mempty
            in  Distr mempty (Cons dx' dy')
        Sub sx     -> Distr a' (Sub (down sx (desub dc) a))

    next :: Monoid a => Struct -> Bool -> Distr a -> a -> (Distr a, a)
    next (Distr sz sc) isLast (Distr a dc) a' = case sc of
        _ | sz == 0 && not isLast
                    -> ( Distr mempty dc, a <> a' )
        Leaf        -> ( Distr a' dc, a )
        Cons sx sy  ->
            let (dx, dy) = deprod dc
                (dx', b) = next sx False dx a'
                (dy', c) = next sy isLast dy b
            in  ( Distr mempty (Cons dx' dy'), a <> c )
        Sub sx      ->
            let (dx', b) = next sx True (desub dc) a'
            in  ( Distr a' (Sub (narrowLast sx dx' b)), a)


                -- ( Distr a' (Sub dx''), a )

    prev :: Monoid a => Struct -> Distr a -> a -> (Distr a, a)
    prev (Distr sz sc) (Distr a dc) a' = case sc of
        _ | sz == 0 -> ( Distr mempty dc, a <> a' )
        Leaf        -> ( Distr a' dc, a )
        Cons sx sy  ->
            let (dx, dy) = deprod dc
                (dy', b) = prev sy dy a'
                (dx', c) = prev sx dx b
            in  ( Distr mempty (Cons dx' dy'), a <> c )
        Sub sx      ->
            let (dx', b) = prev sx (desub dc) a'
            in  ( Distr a' (Sub (narrowFirst sx dx' b)), a)

narrowLast :: Monoid a => Struct -> Distr a -> a -> Distr a
narrowLast (Distr _ sc) (Distr a dc) a' = case sc of
    Cons sx sy -> let (dx, dy) = deprod dc
                  in if size sy > 0 || True
                     then Distr a (Cons dx (narrowLast sy dy a'))
                     else Distr a (Cons (narrowLast sx dx a') dy)
    _          -> Distr (a <> a') dc

narrowFirst :: Monoid a => Struct -> Distr a -> a -> Distr a
narrowFirst (Distr _ sc) (Distr a dc) a' = case sc of
    Cons sx sy -> let (dx, dy) = deprod dc
                  in if size sx > 0
                     then Distr a (Cons (narrowFirst sx dx a') dy)
                     else Distr a (Cons dx (narrowFirst sy dy a'))
    _          -> Distr (a <> a') dc

rail :: Monoid a => Struct -> Distr a -> Distr a
rail st = rail' st True

rail' :: Monoid a => Struct -> Bool -> Distr a -> Distr a
rail' (Distr _ sc) isLast d@(Distr a dc) = case sc of
    Leaf       -> flatten d
    Cons sx sy | size sx == 0 ->
        let (dx, dy) = deprod dc
            dy'      = narrowFirst sy (rail' sy isLast dy) (flatten' dx)
        in  Distr a (Cons mempty dy')
    Cons sx sy | size sy == 0 && not isLast ->
        let (dx, dy) = deprod dc
            dx'      = narrowLast sx (rail' sx False dx) (flatten' dy)
        in  Distr a (Cons dx' mempty)
    Cons sx sy ->
        let (dx, dy) = deprod dc
            dx'      = rail' sx False dx
            dy'      = rail' sy isLast dy
        in  Distr a (Cons dx' dy')
    Sub sx ->
        let dx' = rail' sx True $ case dc of
                Cons dx dy -> dx <> dy
                _          -> desub dc
        in  Distr a (Sub dx')


-- |Scaffold

data Scaffold a = Scaffold
    { uniq  :: Unique
    , value :: Maybe a
    , clamp :: Clamp a
    }

data Clamp a where
    Coerce :: Scaffold b -> Clamp a
    Edge   :: Clamp a
    Pair   :: Scaffold a -> Scaffold b -> Clamp (a, b)
    Inject :: Either (Scaffold a) (Scaffold a) -> Clamp a
    Single :: Scaffold a -> Clamp a

instance Show (Clamp a) where
    show (Coerce _) = "Coerce"
    show Edge       = "Edge"
    show (Pair _ _) = "Pair"
    show (Inject _) = "Inject"
    show (Single _) = "Single"

novalue :: Unique -> Scaffold a
novalue u = Scaffold u Nothing Edge

decoerce :: Unique -> Scaffold a -> Maybe (Scaffold b)
decoerce u (Scaffold u' ma (Coerce sb)) | u == u'
             = Just (unsafeCoerce sb)
decoerce u _ = traceShow u Nothing

coerceSC :: Unique -> Iso a b -> Scaffold a -> Scaffold b
coerceSC u iso sb = Scaffold u (value sb >>= apply iso) (Coerce sb)

depair :: Scaffold (a, b) -> (Scaffold a, Scaffold b)
depair (Scaffold _ _ (Pair sa sb)) = (sa, sb)

pairSC :: Unique -> Scaffold a -> Scaffold b -> Scaffold (a, b)
pairSC u sa sb = Scaffold u (zipMaybe (value sa) (value sb)) (Pair sa sb)

extract :: Scaffold a -> Either (Scaffold a) (Scaffold a)
extract (Scaffold _ _ (Inject ei)) = ei

inL :: Unique -> Scaffold a -> Scaffold a
inL u sa = Scaffold u (value sa) (Inject (Left sa))

inR :: Unique -> Scaffold a -> Scaffold a
inR u sa = Scaffold u (value sa) (Inject (Right sa))

plusSC :: Scaffold a -> Scaffold a -> Scaffold a
plusSC (value -> Nothing) sa = sa
plusSC sa                 _  = sa

desingle :: Scaffold a -> Scaffold a
desingle (Scaffold _ _ (Single sa)) = sa

edgeSC :: Unique -> a -> Scaffold a
edgeSC u a = Scaffold u (Just a) Edge

newtype Scaffolder a = Scaffolder
    { scaffolder :: Blueprint -> a -> Maybe (Scaffold a) }

instance IsoFunctor Scaffolder where
    iso <$> sc = Scaffolder $ \(BIsoMap u bp) a -> do
        b  <- unapply iso a
        sb <- scaffolder sc bp b
        return $ Scaffold u (Just a) (Coerce sb)

instance ProductFunctor Scaffolder where
    sc <*> sd = Scaffolder $ \(BProduct u bp bq) (a, b) -> do
        sa <- scaffolder sc bp a
        sb <- scaffolder sd bq b
        return $ Scaffold u (Just (a, b)) (Pair sa sb)

instance Alter Scaffolder where
    empty     = Scaffolder $ \_ a -> Nothing
    sc <|> sd = Scaffolder $ \(~(BAlter u bp bq)) a ->
        (do sa <- scaffolder sc bp a
            return $ Scaffold u (Just a) (Inject (Left sa)))
        `mplus`
        (do sa <- scaffolder sd bq a
            return $ Scaffold u (Just a) (Inject (Right sa)))

instance Syntax Scaffolder where
    pure a = Scaffolder $ \(BEnd u) a' ->
        if a == a' then return $ Scaffold u (Just a) Edge
                   else Nothing
    char = Scaffolder $ \(BEnd u) c ->
        return $ Scaffold u (Just c) Edge

    part d sc = Scaffolder $ \(BNest u bp) a -> do
        sa <- scaffolder sc bp a
        return $ Scaffold u (Just a) (Single sa)

-- |Con

data Con f a = Con
    { fin    :: f a
    , subCon :: SubCon f a
    }

data SubCon f a where
    IsoMap  :: Iso a b -> Con f a -> SubCon f b
    Product :: Con f a -> Con f b -> SubCon f (a, b)
    Alter   :: Con f a -> Con f a -> SubCon f a
    Empty   :: SubCon f a
    -- Pure    :: a -> Con f a
    End     :: SubCon f a
    -- Token   :: Con f Char
    Nest    :: Con f a -> SubCon f a


    -- Rec     :: Int -> Con f a

type Unique    = Int
type UniqueMap = IntMap

-- newtype Blueprinter a = Blueprinter (UniqueMap Int -> IO (Con f a))

instance IsoFunctor f => IsoFunctor (Con f) where
    iso <$> c = Con (iso <$> fin c) (IsoMap iso c)

instance ProductFunctor f => ProductFunctor (Con f) where
    c <*> d = Con (fin c <*> fin d) (Product c d)

instance Alter f => Alter (Con f) where
    empty   = Con empty End
    c <|> d = Con (fin c <|> fin d) (Alter c d)

instance Syntax f => Syntax (Con f) where
    pure a   = Con (pure a) End
    char     = Con char End
    part d c = Con (part d $ fin c) (Nest c)


pattern BIsoMap u x <-
    ( id Arrow.&&& (\(subCon -> IsoMap _ x) -> unsafeCoerce x)
    -> (Con (Const u) _, x :: Blueprint))

pattern BProduct u x y <-
    ((unsafeCoerce :: Blueprint -> Con (Const Unique) ((), ()))
    -> Con (Const u) (Product x y))

pattern BAlter u x y <- Con (Const u) (Alter x y)

pattern BNest u x <- Con (Const u) (Nest x)

pattern BEnd u <- Con (Const u) End

type Blueprint = Con (Const Unique) ()

showB :: Blueprint -> String
showB = go IntSet.empty
  where
    go :: IntSet.IntSet -> Con (Const Unique) a -> String
    go us (Con (Const u) sc) =
        let us' = IntSet.insert u us
            go' :: forall a. Con (Const Unique) a -> String
            go' a = if u `IntSet.member` us
                    then ""
                    else go us' a
            s   = case sc of
                IsoMap _ x  -> "IsoMap" ++ go' x
                Product x y -> "Alter"  ++ go' x ++ go' y
                Alter x y   -> "Alter"  ++ go' x ++ go' y
                Empty       -> "Empty"
                End         -> "End"
                Nest x      -> "Nest" ++ go' x
        in "(" ++ show u ++ " " ++ s ++ ")"

unique :: Blueprint -> Unique
unique (Con (Const u) _) = u

coerceBProduct :: Blueprint -> Con (Const Unique) ((), ())
coerceBProduct = unsafeCoerce

coerce :: SubCon (Const Unique) a -> SubCon (Const Unique) b
coerce = unsafeCoerce

getBlueprint :: forall a. Con BP a -> IO Blueprint
getBlueprint c = do
    (b, m) <- runStateT (go c) IntMap.empty
    _ <- getUnique ()
        -- (do let Con p _ = char :: Con BP Char
        --     tu <- lift $ getUnique p
        --     modify $ IntMap.insert tu (Con (Const tu) End)
        --     b  <- go c
        --     return (tu, b)) IntMap.empty
    trace (showB b) $ return b
  where
    go :: forall a. Con BP a -> StateT (UniqueMap Blueprint) IO Blueprint
    go (Con BPChar sc) = return $ Con (Const tokenUnique) End
    go (Con p sc) = mdo
        u   <- lift $ getUnique p
        bm  <- get
        mbp <- IntMap.lookup u `fmap` get
        modify $ IntMap.insert u bp
        bp  <- case mbp of
            Just b  -> return b
            Nothing -> Con (Const u) `fmap` case sc of
                IsoMap iso c -> IsoMap id `fmap` go c
                Product c d  -> do
                    bx <- go c
                    by <- go d
                    return . coerce $ Product bx by
                Alter c d -> do
                    bx <- go c
                    by <- go d
                    return $ Alter bx by
                Empty -> return Empty
                End   -> return End
                Nest c -> Nest `fmap` go c
                -- _            -> return End
        return bp

tokenUnique :: Unique
tokenUnique = 0

getUnique :: a -> IO Unique
getUnique a = hashStableName `fmap` makeStableName a

data BP a = BPChar
          | BPZero
          | BPOne (BP a)
          | BPTwo (BP a) (BP a)

instance IsoFunctor BP where
    iso <$> b = BPOne $ unsafeCoerce b

instance ProductFunctor BP where
    (<*>) = unsafeCoerce BPTwo

instance Alter BP where
    empty = BPZero
    (<|>) = unsafeCoerce BPTwo

instance Syntax BP where
    pure a   = BPZero
    char     = BPChar
    part _   = BPOne

-- data DynStableName = DynStableName (StableName ())

-- hashDynStableName :: DynStableName -> Int
-- hashDynStableName (DynStableName sn) = hashStableName sn

-- instance Eq DynStableName where
--     (DynStableName sn1) == (DynStableName sn2) = sn1 == sn2

-- makeDynStableName :: a -> IO DynStableName
-- makeDynStableName a = do
--     st <- makeStableName a
--     return $ DynStableName (unsafeCoerce st)
-- mapEnd :: (f a -> g a) -> Con f a -> Con g a
-- mapEnd = mapEnd

-- recursive :: Con f a -> IO (Con f a)
-- recursive = recursive

-- construct :: Syntax f => Con f a -> f a
-- construct = \case
--     End     u fa      -> fa
--     IsoMap  u iso eqa -> iso <$> construct eqa
--     Product u eqa eqb -> construct eqa <*> construct eqb
--     Alter   u eqa eqb -> construct eqa <|> construct eqb
--     Nest    u


data Fragment f where
    Fragment :: Unique -> f a -> Fragment f

toFrag :: Unique -> f a -> Fragment f
toFrag = Fragment

fromFrag :: Unique -> Fragment f -> Maybe (f a)
fromFrag u (Fragment u' fa) | u == u' = Just $ unsafeCoerce fa
fromFrag _ _                = Nothing


-- move :: Monoid a => Struct -> Pos.Path -> Distr a -> Distr a
-- move st (Pos.Path u x p) = nexts x st . ups' u
--   where
--     ups' :: Monoid a => Int -> Distr a -> Distr a
--     ups' 0 d = d
--     ups' n d = let (Distr a dc, us) = ups n d
--                in  Distr (a <> mconcatF us) dc

--     ups :: Monoid a => Int -> Distr a -> (Distr a, Seq.Seq a)
--     ups n (Distr a dc) =
--         let (dc', u Seq.:<| us) = case dc of
--                 Leaf       -> (mempty, emptyNSeq)
--                 Cons dx dy ->
--                     let (dx', uxs) = ups n dx
--                         (dy', uys) = ups n dy
--                     in  ( Cons dx' dy'
--                         , Seq.zipWith (<>) uxs uys)
--                 Sub dx     -> mapFst Sub $ ups n dx
--         in (Distr u dc', us Seq.|> a)
--       where
--         emptyNSeq :: Monoid a => Seq.Seq a
--         emptyNSeq = Seq.replicate n mempty

--     nexts :: Monoid a => Int -> Struct -> Distr a -> Distr a
--     nexts x st@(Distr _ sc) d@(Distr a dc) = case sc of
--         Leaf     -> flatten d
--         Cons _ _ -> let (Distr a' dc, ns) = consNexts st d emptyXSeq
--                     in  Distr (a' <> mconcatF ns) dc
--         Sub sx   -> Distr a (Sub (nexts x sx (desub dc)))
--       where
--         consNexts :: Monoid a
--                   => Struct -> Distr a -> Seq.Seq a -> (Distr a, Seq.Seq a)
--         consNexts (Distr _ (Cons sx sy)) (Distr a dc) ns | x > 0 =
--             let (dx, dy)   = deprod dc
--                 (dx', nxs) = consNexts sx dx ns
--                 (dy', nys) = consNexts sy dy nxs
--             in  ( Distr a (Cons dx' dy')
--                 , nys )
--         consNexts (Distr _ (Cons sx sy)) (Distr a dc) ns =
--             let (dx, dy)   = deprod dc
--                 (dy', nys) = consNexts sy dy ns
--                 (dx', nxs) = consNexts sx dx nys
--             in  ( Distr a (Cons dx' dy')
--                 , nxs )
--         consNexts st d ns
--             | size st == 0 = ( d, ns )
--         consNexts st d ns  =
--             let Distr a dc = nexts x st d
--                 (nxs, nys) = Seq.splitAt (size st) ns
--             in  ( Distr (mconcatF nxs) dc
--                 , nys Seq.|> a )

--         emptyXSeq :: Monoid a => Seq.Seq a
--         emptyXSeq = Seq.replicate (abs x) mempty

--     downs :: Pos.Position -> Struc -> Distr a -> Distr a
--     downs Seq.Empty _ d = d
--     downs pos (Distr _ sc) d = case sc of
--         Cons sx sy ->

-- consStruct :: Struct -> Struct -> Struct
-- consStruct sta stb = Struct (size sta + size stb) False (Cons sta stb)

-- uncons :: Struct -> Pos.Distr a
--           -> ((Struct, Struct), (Pos.Distr a, Pos.Distr a))
-- uncons st p =
--     let Cons sta stb = substract st
--         (px, py)        = Pos.split (size sta) p
--         px'             = applyWhen (isSingle sta) Pos.head px
--         py'             = applyWhen (isSingle stb) Pos.head py
--     in  ((sta, stb), (px', py'))

-- cons :: (Struct, Struct) -> (Pos.Distr a, Pos.Distr a)
--      -> Pos.Distr a
-- cons (sta, stb) (px, py) =
--     Pos.merge (size sta)
--     (applyWhen (isSingle sta) Pos.singleton px)
--     (applyWhen (isSingle stb) Pos.singleton py)


-- product :: (Struct, Struct) -> (Pos.Distr a, Pos.Distr a)
--         -> (Struct, Pos.Distr a)
-- product (sta, stb) (px, py) =
--     ( Struct (size sta + size stb) False $ Product' sta stb
--     , Pos.merge (size sta) px py)


-- railTop :: Struct -> Pos.Carets -> Pos.Carets
-- railTop st cs = Pos.onRoots (Pos.roots cs)
--              <> rail' st (Pos.narrow 0 (size st - 1) cs)

-- rail' :: Struct -> Pos.Distr () -> Pos.Distr ()
-- rail' st p = case substract st of
--     Unit     -> Pos.bottom p
--     Sub st'  -> Pos.onRoots (Pos.roots p)
--              <> rail' st' (Pos.narrow 0 (size st' - 1) $ Pos.exceptRoots p)
--     Cons _ _ ->
--         if Pos.null p then p
--         else let ((sta, stb), (px, py)) = uncons st p
--              in  cons (sta, stb) (rail' sta px, rail' stb py)




-- newtype Gatherer f d a = Gatherer
--     { gatherWith :: forall g. (Syntax g, Syntax  => g -> f -> () }


-- instance IsoFunctor (Distributor d p) where
--     iso <$> Distributor distr = Distributor $
--         \mapper fa d -> iso <$> distr mapper (iso >$< fa) d

-- instance ProductFunctor (Distributor d p) where
--     Distributor distr <*> Distributor distr' = Distributor $
--         \mapper fab d -> case d of
--             DCons a b -> _

-- data Equivalent f g a where
--     BottomEq  :: f a -> g a -> Equivalent f g a
--     IsoMapEq  :: Iso b a -> Equivalent f g b -> Equivalent f g a
--     ProductEq :: Equivalent f g a -> Equivalent f g b -> Equivalent f g (a, b)
--     AlterEq   :: Equivalent f g a -> Equivalent f g a -> Equivalent f g a
--     EmptyEq   :: Equivalent f g a
    -- RecEq     ::

-- type Equivalent f g = Con (Equiv f g)

-- data Equiv f g a = Pair (f a) (g a)

-- instance (IsoFunctor f, IsoFunctor g) => IsoFunctor (Equiv f g) where
--     iso <$> Pair fa ga = Pair (iso <$> fa) (iso <$> ga)

-- instance (ProductFunctor f, ProductFunctor g) =>
--          ProductFunctor (Equiv f g) where
--     Pair fa ga <*> Pair fb gb = Pair (fa <*> fb) (ga <*> gb)

-- instance (Alter f, Alter g) => Alter (Equiv f g) where
--     empty = Pair empty empty
--     Pair fa ga <|> Pair fb gb = Pair (fa <|> fb) (ga <|> gb)

-- instance (Syntax f, Syntax g) => Syntax (Equiv f g) where
--     pure a = Pair (pure a) (pure a)
--     char   = Pair char char
--     part d (Pair fa ga) = Pair (part d fa) (part d ga)

-- reflexive :: Equivalent f f a
-- reflexive = reflexive

-- symmetric :: Equivalent f g a -> Equivalent g f a
-- symmetric = symmetric

-- transitive :: Equivalent f g a -> Equivalent g h a -> Equivalent f h a
-- transitive = transitive

-- runEquivalent :: (forall x. f x -> g x -> r x)
--               -> Equivalent f g a
--               -> Equivalent r g a
-- runEquivalent run = mapEnd $ \(Pair fx gx) -> Pair (run fx gx) gx

    -- Equivalent $ case c of
    -- Pure (fa, ga)   -> Pure (run fa ga, ga)
    -- IsoMap iso eqa  -> IsoMap iso $ runEquivalent run eqa
    -- Product eqa eqb -> runEquivalent run eqa `Product` runEquivalent run eqb
    -- Alter   eqa eqb -> runEquivalent run eqa `Alter`   runEquivalent run eqb
    -- Empty           -> Empty

-- fromEquivalent :: (Syntax f, Syntax g) => Equivalent f g a -> (f a, g a)
-- fromEquivalent eq = let Pair fa ga = construct eq
--                     in  (fa, ga)

-- end :: f a -> g a -> Equivalent f g a
-- end fa ga = End $ Pair fa ga

-- endConst :: c -> g a -> Equivalent (Const c) g a
-- endConst = end . Const

-- newtype Foo a = Foo (String -> Diff a)

-- foo :: String -> Equivalent Foo Diff a
-- foo = foo

-- newtype Bar a = Bar (Diff a -> String)

-- bar :: Equivalent Bar Diff a -> Equivalent (Const String) Diff a
-- bar = runEquivalent $ \(Bar f) d -> Const $ f d


-- newtype Printer a = Printer (Diff a -> Diff String)


-- foobar :: Equivalent Foo Bar a
-- foobar = _



data Isorecursive

    -- Sub     :: Description -> Archivist f a -> Con f a

-- instance IsoFunctor f => IsoFunctor (Archivist f) where
--     iso <$> ac = Archivist (iso <$> finished ac) (IsoMap iso ac)

-- instance ProductFunctor f => ProductFunctor (Archivist f) where
--     aca <*> acb = Archivist (finished aca <*> finished acb) (Product aca acb)

-- instance Alter f => Alter (Archivist f) where
--     empty       = Archivist empty Pure
--     aca <|> acb = Archivist (finished aca <|> finished acb) (Alter aca acb)

-- instance Syntax f => Syntax (Archivist f) where
--     pure a    = Archivist (pure a) Pure
--     char      = Archivist char Pure
    -- part d ac = Archivist (part d $ finished ac) (Sub d ac)

-- packtrat :: Archivist f a -> f a
-- packtrat a = case constitution a of
--     Alter l r -> _

data Mapped c f a = forall b. Mapped (c a b) (f b)

mapmap :: Category c => c a b -> Mapped c f b -> Mapped c f a
mapmap f (Mapped g b) = Mapped (g . f) b

-- unmapIso :: Archivist f b -> Mapped Iso (Archivist f) b
-- unmapIso a = case constitution a of
--     IsoMap iso b -> mapmap (inverse iso) $ unmapIso b
--     _            -> Mapped id a

-- type Diff = Con Maybe

-- data Diff a = None
--             | All a

-- toMaybe :: Diff a -> Maybe a
-- toMaybe None    = Nothing
-- toMaybe (All a) = Just a

-- toDiff  :: Maybe a -> Diff a
-- toDiff  Nothing  = None
-- toDiff  (Just a) = All a

-- data Diff a where
--     None  :: Diff a
--     All   :: a -> Diff a
--     Inner :: Iso b a -> Diff b -> Diff a
--     Both  :: Diff a -> Diff b -> Diff (a, b)

-- instance IsoFunctor Diff where

-- instance ProductFunctor Diff where

-- instance Alter Diff where

-- instance Syntax Diff where



-- data Incremental p r a =
--     Incremental (Diff (p a) -> (Diff (r a), Incremental p r a))

-- newtype Initializer p r a =
--     Initializer { initialize :: a -> Incremental p r a }

-- -- incIsoMap :: Iso a b -> Initializer p r a -> Initializer p r b
-- -- incIsoMap = _

-- newtype Accumulator a = Accumulator (a -> Diff a -> a)


-- incProduct :: (p (a, b) -> s (a, b) -> (s (a, b), r (a, b)))
--            -> Initializer p r a
--            -> Initializer p r b
--            -> Initializer p r (a, b)
-- incProduct update initp initq =
--     Initializer $ \(a, b) ->
--     inc (initialize initp a) (initialize initq b)
--   where
--     inc incp incq = Incremental $ \case
--         All pab ->
--             let (sab', rab) = update pab sab
--             in  (All rab, inc sab' incp incq)
--         Both dpa dpb ->
--             let (dra, incp') = incp dpa
--                 (drb, incq') = incq dpb
--             in  (Both dra drb, inc (accum sab ))
--         _ -> (None, inc sab incp incq)

-- incChar :: (p Char -> s Char)
--          -> (p Char -> s Char -> (s Char, r Char))
--          -> Initializer p r Char
-- incChar ini update =
--     Initializer $ \pc -> inc $ ini pc
--   where
--     inc sc = Incremental $ \case
--         All pc -> let (sc', rc) = update pc sc'
--                   in  (All rc, inc sc')
--         _     -> (None, inc sc)

data ReifyF a
    = RUnit
    | RSub a
    | RProduct a a
    | REmpty
    | RAlter a a
    deriving (Show, Functor, Foldable, Traversable)

newtype Reify a = Reify (ReifyF (Reify a))

-- type Reify a = Reify

-- type Reify =  Reify

-- data SyntaxGraph a
--     = GUnit
--     | GSub a
--     | GProduct a a
--     | GAlter a a
--     deriving Show

-- instance MuRef (Reify a) where
--     type DeRef (Reify a) = ReifyF
--     mapDeRef f (Reify r) = traverse f r

-- instance IsoFunctor Reify where
--     iso <$> r = unsafeCoerce r

-- instance ProductFunctor Reify where
--     rx <*> ry = Reify $ unsafeCoerce $ RProduct rx (unsafeCoerce ry)

-- instance Alter Reify where
--     empty = Reify REmpty
--     rx <|> ry = Reify $ RAlter rx ry

-- instance Syntax Reify where
--     pure _ = Reify RUnit
--     char   = Reify RUnit
--     part _ r = Reify $ RSub r

-- reify' :: forall a. (forall f. Syntax f => f a) -> IO (Graph ReifyF)
-- reify' p = reifyGraph (p :: Reify a)

-- reify :: Archivist f a -> IO ([(Int, String)], String)
-- reify =  go []
--   where
--     go :: [(Int, String)] -> Archivist f a -> IO ([(Int, String)], String)
--     go xs a = do
--         hn <- hashStableName `fmap` makeStableName a
--         case lookup hn xs of
--             Just s  -> return (xs, "(R " ++ show hn ++ ")")
--             Nothing ->
--                 case constitution a of
--                     IsoMap _ a' -> do
--                         (xs', s) <- go ((hn, "") : xs) a'
--                         let s' = "(M " ++ s ++ ")"
--                         return ((hn, s') : xs', s')
--                     Product a' b' -> do
--                         (xs',  sa) <- go ((hn, "") : xs) a'
--                         (xs'', sb) <- go xs' b'
--                         let s' = "(P " ++ sa ++ " " ++ sb ++ ")"
--                         return ((hn, s') : xs'', s')
--                     Alter a' b' -> do
--                         (xs',  sa) <- go ((hn, "") : xs) a'
--                         (xs'', sb) <- go xs' b'
--                         let s' = "(A " ++ sa ++ " " ++ sb ++ ")"
--                         return ((hn, s') : xs'', s')

--                     _ -> return (xs, "U")



class Syntax f => Intermediate f where
    type Param f a
    type Result f a
    -- isomap :: Iso alpha beta ->
    runItm :: f a -> Param f a -> Maybe (Result f a)

-- instance Intermediate Radiographer where
--     type Param  Radiographer a = a
--     type Result Radiographer a = Struct
--     runItm = xray


-- monoPoly :: c f => (forall g. c g => g a) -> Poly c f a -> f a
-- monoPoly p = runPoly p $ poly p

-- runPoly :: c f => f a -> Poly c f a -> f a
-- runPoly p (Poly pf) = pf p

-- instance c f => forall c. c (Poly c f) where


data Packrat f a = Packrat (f a)

-- class RecDescent f where
--     isomap  :: Iso a b -> f a -> f b
--     pure    :: Eq a => a -> f a
--     token   :: f Char
--     product :: (Struct, Struct) -> f a -> f b -> f (a, b)
--     chioce  :: (Struct, Struct) -> f a -> f b -> f (Either a b)

-- data RecDesc f a = RecDesc Bool Int (f a)

-- instance RecDescent f => IsoFunctor (RecDesc f) where
--     iso <$> RecDesc s x fa = RecDesc s x $ isomap iso fa

-- instance RecDescent f => ProductFunctor (RecDesc f) where
--     RecDesc _ x fa <*> RecDesc _ y fb =
--         RecDesc False (x + y) $ product (Struct x False, Struct y False) fa fb
-- -- instance (RecDescent f) Syntax where

-- instance RecDescent f => Alter (RecDesc f) where
--     RecDesc _ x fa <|> RecDesc _ y fb =
