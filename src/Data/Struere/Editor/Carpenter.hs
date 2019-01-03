
module Data.Struere.Editor.Carpenter where


import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Prelude                      hiding (id, pure, (*>), (<$>),
                                               (<*), (<*>), (<|>))
-- import           Data.Number.Nat
import           Data.Dynamic
import           Debug.Trace

import           Data.Struere.Editor.Brick
import qualified Data.Struere.Editor.Position as Pos
import           Data.Struere.Editor.Util
import           Data.Struere.Isomorphism
import           Data.Struere.Structural
-- import           Data.Struere.Viewer

-- Positioned


-- type Positioned a = [Target a]

-- data Target a = Root a
--               | Sub Int (Positioned a)
--               deriving (Show, Functor)

-- mapPos :: (a -> b) -> Positioned a -> Positioned b
-- mapPos = fmap . fmap

-- unconsRoot :: Positioned a -> ([a], Positioned a)
-- unconsRoot = foldr
--     (\t (as, ss) -> case t of
--             Root a -> (a:as,     ss)
--             sub    -> (  as, sub:ss))
--     ([], [])

-- splitSub :: Nat -> Positioned a -> (Positioned a, Positioned a)
-- splitSub n = foldr
--     (\t (ls, rs) -> case t of
--             Root _                  -> (ls, rs)
--             Sub x p | x < fromNat n -> (Sub x p : ls, rs)
--                     | otherwise     -> (ls, Sub (x - fromNat n) p : rs))
--     ([], [])

-- mergeSub :: Nat -> Positioned a -> Positioned a -> Positioned a
-- mergeSub n ls rs = ls ++ map (shiftSub $ fromNat n) rs

-- singletonSub :: Positioned a -> Positioned a
-- singletonSub [] = []
-- singletonSub p  = [Sub 0 p]

-- headSub :: Positioned a -> Positioned a
-- headSub = (concat .) . mapMaybe $ \case
--     Sub 0 p -> Just p
--     _       -> Nothing

-- shiftSub :: Int -> Target a -> Target a
-- shiftSub n (Sub x p) = Sub (x + n) p
-- shiftSub _ t         = t

-- -- subs :: Positioned a -> [Positioned a]
-- -- subs

-- headPos :: Positioned a -> Target a
-- headPos = head . headSub


-- enumerate :: Positioned a -> [(Position, a)]
-- enumerate = concatMap $ \case
--     Root a  -> [(rootPos, a)]
--     Sub x p -> map (mapFst $ subPos x) $ enumerate p


-- Position



-- Carpenter

type Instrs = Pos.Positioned Instr

data Instr = IDelete
           | ISet Dynamic
           deriving (Show)

type Bubbles = Pos.Positioned Bubble

data Bubble = Unaccepted
            | BDelete

type BrickDiff = Pos.Positioned Brick

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

        , updater = \is ->
                if Pos.null is then (mab, mempty, next mab cp cq)
                else let (lis, ris)    = Pos.split (size cp) is
                         lis'          = applyIf (size cp <= 1) Pos.head lis
                         ris'          = applyIf (size cq <= 1) Pos.head ris
                         (ma, bd, cp') = updater cp lis'
                         (mb, be, cq') = updater cq ris'
                         mab'          = zipMaybe ma mb
                         bd'           = applyIf (size cp <= 1) Pos.singleton bd
                         be'           = applyIf (size cq <= 1) Pos.singleton be
                     in  ( mab'
                         , mergeBrickDiff (size cp) bd' be'
                         , next mab' cp' cq'
                         )
        }

instance Alter Carpenter where
    empty = Carpenter 0 (const Nothing) (const (Nothing, mempty, empty))
    -- cp@(Carpenter n bf uf) <|> cq@(Carpenter m bg ug)
    (<|>) = flip fix Nothing $ \next mab cp cq -> Carpenter
        { size    = size cp `max` size cq
        , builder = \a -> case (builder cp a, builder cq a) of
                (Just (ba, cp'), _      ) -> Just (ba, next (Just a) cp' cq )
                (_,       Just (bb, cq')) -> Just (bb, next (Just a) cp  cq')
                (Nothing, Nothing       ) -> Nothing
        , updater = \is ->
                if Pos.null is then (mab, mempty, next mab cp cq)
                else let (ma, bd, cp') = updater cp is
                         (mb, be, cq') = updater cq is
                         mab'          = ma `mplus` mb
                     in  (mab', bd <> be, next mab' cp' cq')
        }

instance Structural Carpenter where
    pure a = Carpenter
        { size    = 0
        , builder = \a' -> if a == a' then Just (Empty, pure a) else Nothing
        , updater = const (Just a, mempty, pure a)
        }
    char = flip fix Nothing $ \next mc -> Carpenter
        { size = 1
        , builder = \c -> Just (Plane c, next $ Just c)
        , updater = \is -> case changes is of
                NoChange -> (mc, mempty, next mc)
                Set c    -> (Just c,  Pos.root (Plane c), next $ Just c)
                Delete   -> (Nothing, mempty, next Nothing)
        }
    sub l = flip fix Nothing $ \next ma cp -> Carpenter
        { size = 1
        , builder = \a -> do
                (br, cp') <- builder cp a
                return (br, next (Just a) cp')
        , updater = \is ->
                case changes is of
                    NoChange ->
                        let sis            = applyIf (size cp <= 1) Pos.head is
                            (ma', bd, cp') = updater cp sis
                        in  (ma', bd, next ma' cp')
                    Set a    -> fromMaybe (ma, mempty, next ma cp) $ do
                        (br, cp') <- builder cp a
                        return (Just a, Pos.root br, next (Just a) cp')
                    Delete   -> (Nothing, mempty, next Nothing cp)
        }

data Changes a = NoChange
               | Set a
               | Delete
               deriving (Show)

changes :: Typeable a => Instrs -> Changes a
changes is = case Pos.roots is of
    IDelete : _ -> Delete
    -- ISet d  : _ -> maybe NoChange Set $ fromDynamic d
    ISet d  : _ -> maybe NoChange Set $ fromDynamic d
    _           -> NoChange

mergeBrickDiff :: Nat -> BrickDiff -> BrickDiff -> BrickDiff
-- mergeBrickDiff n [Root (Plane xs)] [Root (Plane ys)] = [Root $ Plane $ xs ++ ys]
mergeBrickDiff n x y                                 = Pos.merge n x y

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
