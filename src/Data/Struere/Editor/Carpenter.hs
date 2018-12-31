{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module Data.Struere.Editor.Carpenter where


import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Prelude                      hiding (id, pure, (*>), (<$>),
                                               (<*), (<*>), (<|>))
-- import           Data.Number.Nat
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Dynamic
import           Debug.Trace
import           GHC.Generics

import           Data.Struere.Editor.Brick
import           Data.Struere.Editor.Protocol
import           Data.Struere.Isomorphism
import           Data.Struere.Structural
-- import           Data.Struere.Viewer

-- Positioned

type Positioned a = [Target a]

data Target a = Root a
              | Sub Int (Positioned a)
              deriving (Show)

unconsRoot :: Positioned a -> ([a], Positioned a)
unconsRoot = foldr
    (\t (as, ss) -> case t of
            Root a -> (a:as,     ss)
            sub    -> (  as, sub:ss))
    ([], [])

splitSub :: Nat -> Positioned a -> (Positioned a, Positioned a)
splitSub n = foldr
    (\t (ls, rs) -> case t of
            Root _                  -> (ls, rs)
            Sub x p | x < fromNat n -> (Sub x p : ls, rs)
                    | otherwise     -> (ls, Sub (x - fromNat n) p : rs))
    ([], [])

mergeSub :: Nat -> Positioned a -> Positioned a -> Positioned a
mergeSub n ls rs = ls ++ map (shiftSub $ fromNat n) rs

singletonSub :: Positioned a -> Positioned a
singletonSub [] = []
singletonSub p  = [Sub 0 p]

headSub :: Positioned a -> Positioned a
headSub = (concat .) . mapMaybe $ \case
    Sub 0 p -> Just p
    _       -> Nothing

shiftSub :: Int -> Target a -> Target a
shiftSub n (Sub x p) = Sub (x + n) p
shiftSub _ t         = t

enumerate :: Positioned a -> [(Position, a)]
enumerate = concatMap $ \case
    Root a  -> [(rootPos, a)]
    Sub x p -> map (mapFst $ subPos x) $ enumerate p

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)
-- History

-- type History s = [Diff s]

-- data Diff s = Diff
--     { pos   :: [Int]
--     , value :: s
--     } deriving (Show, Generic)


-- instance ToJSON   a => ToJSON   (Diff a)
-- instance FromJSON a => FromJSON (Diff a)


-- Position



-- Carpenter

type Instrs = Positioned Instr

data Instr = IChar Char
           | IDelete
           | ISet Dynamic
           deriving (Show)

type Bubbles = Positioned Bubble

data Bubble = Unaccepted
            | BDelete

type BrickDiff = Positioned Brick

data Carpenter a = Carpenter
    { size    :: Nat
    , builder :: a -> Maybe (Brick, Carpenter a)
    , updater :: Instrs -> (Maybe a, BrickDiff, Carpenter a)
    }

instance IsoFunctor Carpenter where
    iso <$> cp = Carpenter
        { size = size cp
        , builder = \b -> do
                a <- unapply iso b
                (x, cp') <- builder cp a
                return (x, iso <$> cp')
        , updater = \is ->
                let (ma, bd, cp') = updater cp is
                in  (apply iso =<< ma, bd, iso <$> cp')
        }

instance ProductFunctor Carpenter where
    (<*>) = flip fix Nothing $ \next mab cp cq -> Carpenter
        { size = size cq + size cp
        , builder = \(a, b) -> do
                (bd, cp') <- builder cp a
                (be, cq') <- builder cq b
                return (consBrick bd be, next (Just (a, b)) cp' cq')
        , updater = \case
                [] -> (mab, [], next mab cp cq)
                is -> let (lis, ris)    = splitSub (size cp) is
                          lis'          = applyIf (size cp <= 1) headSub lis
                          ris'          = applyIf (size cq <= 1) headSub ris
                          (ma, bd, cp') = updater cp lis'
                          (mb, be, cq') = updater cq ris'
                          mab'          = zipMaybe ma mb
                          bd'           = applyIf (size cp <= 1) singletonSub bd
                          be'           = applyIf (size cq <= 1) singletonSub be
                      in  ( mab'
                          , mergeBrickDiff (size cp) bd' be'
                          , next mab' cp' cq'
                          )
        }

instance Alter Carpenter where
    empty = Carpenter 0 (const Nothing) (const (Nothing, [], empty))
    -- cp@(Carpenter n bf uf) <|> cq@(Carpenter m bg ug)
    (<|>) = flip fix Nothing $ \next mab cp cq -> Carpenter
        { size    = size cp `max` size cq
        , builder = \a -> case (builder cp a, builder cq a) of
                (Just (ba, cp'), _      ) -> Just (ba, next (Just a) cp' cq )
                (_,       Just (bb, cq')) -> Just (bb, next (Just a) cp  cq')
                (Nothing, Nothing       ) -> Nothing
        , updater = \case
                [] -> (mab, [], cp <|> cq)
                is -> let (ma, bd, cp') = updater cp is
                          (mb, be, cq') = updater cq is
                          mab'          = ma `mplus` mb
                      in  (mab', bd ++ be, next mab' cp' cq')
        }

instance Structural Carpenter where
    pure a = Carpenter
        { size    = 0
        , builder = \a' -> if a == a' then Just (Empty, pure a) else Nothing
        , updater = const (Just a, [], pure a)
        }
    char = flip fix Nothing $ \next mc -> Carpenter
        { size = 1
        , builder = \c -> Just (Plane [c], next $ Just c)
        , updater = \is -> case fst $ updates is of
                NoChange -> (mc, [], next mc)
                Set c    -> (Just c,  [Root $ Plane [c]], next $ Just c)
                Delete   -> (Nothing, [], next Nothing)
        }
    sub l = flip fix Nothing $ \next ma cp -> Carpenter
        { size = 1
        , builder = \a -> do
                (br, cp') <- builder cp a
                return (br, next (Just a) cp')
        , updater = \is ->
                let (u, sis) = updates is
                in  case u of
                    NoChange -> let (ma', bd, cp') = updater cp sis
                                 in  (ma', bd, next ma' cp')
                    Set a     -> fromMaybe (ma, [], next ma cp) $ do
                        (br, cp') <- builder cp a
                        return (Just a, [Root br], next (Just a) cp')
                    Delete    -> (Nothing, [], next Nothing cp)
        }

data Changes a = NoChange
               | Set a
               | Delete
               deriving (Show)

updates :: Typeable a => Instrs -> (Changes a, Instrs)
updates is = let (ris, sis) = unconsRoot is
           in  (, sis) $ case ris of
               IDelete : _ -> Delete
               -- ISet d  : _ -> maybe NoChange Set $ fromDynamic d
               ISet d  : _ -> maybe NoChange Set $ fromDynamic d
               _           -> NoChange

mergeBrickDiff :: Nat -> BrickDiff -> BrickDiff -> BrickDiff
mergeBrickDiff n [Root (Plane xs)] [Root (Plane ys)] = [Root $ Plane $ xs ++ ys]
mergeBrickDiff n x y                                 = mergeSub n x y

-- Utilities

zipMaybe :: Maybe a -> Maybe b -> Maybe (a, b)
zipMaybe = liftM2 (,)

unzipMaybe               :: Maybe (a, b) -> (Maybe a, Maybe b)
unzipMaybe Nothing       = (Nothing, Nothing)
unzipMaybe (Just (a, b)) = (Just a, Just b)

plusMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
plusMaybe f (Just x) (Just y) = Just $ f x y
plusMaybe _ Nothing   y       = y
plusMaybe _ x        Nothing  = x

applyIf :: Bool -> (a -> a) -> a -> a
applyIf True  f a = f a
applyIf False _ a = a


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


-- data Builder a = Builder (a -> Maybe (Carpenter a))

-- instance IsoFunctor Builder where
--     iso <$> Builder f
--         = Builder $ \b -> do
--             a <- unapply iso b
--             s <- f a
--             return $ iso <$> s

-- instance ProductFunctor Builder where
--     Builder f <*> Builder g
--         = Builder $ \(a, b) -> do
--             sf <- f a
--             sg <- g b
--             return $ sf <*> sg

-- instance Alter Builder where
--     empty = Builder $ const Nothing
--     Builder f <|> Builder g
--         = Builder $ \a -> f a `mplus` g a


-- instance Structural Builder where
--     char = Builder $ \c ->
--         let sc = Carpenter 1 $ \is _ ->
--                 case fst $ unconsRoot is of
--                     IChar c'   : _ -> Just (c', [], [Root $ Plane [c']], sc)
--                     IDelete    : _ -> Nothing
--                     IReplace d : _ -> Nothing
--                     _              -> Just (c, [], [], sc)
--         in  Just sc
--     pure a = Builder $ \_ ->
--         let sc = Carpenter 1 $ \_ _ -> Just (a, [], [], sc)
--         in  Just sc

-- runBuilder :: Builder a -> a -> Maybe (Carpenter a)
-- runBuilder (Builder f) = f
