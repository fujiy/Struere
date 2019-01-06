
module Data.Struere.Editor.Carpenter where


import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Prelude                      hiding (id, pure, (<$>))
-- import           Data.Number.Nat
import           Data.Dynamic
import           Debug.Trace

import           Data.Struere.Editor.Brick
import qualified Data.Struere.Editor.Position as Pos
import           Data.Struere.Editor.Util
import           Data.Struere.Isomorphism
import           Data.Struere.Structural
-- import           Data.Struere.Viewer

data Struct = Struct
    { size   :: Int
    , single :: Bool
    }

-- Carpenter

type Instrs = Pos.Positioned Instr

data Instr = IDelete
           | ISet Dynamic
           deriving (Show)

-- type Bubbles = Pos.Positioned Bubble

-- data Bubble = Unaccepted
--             | BDelete

type BrickDiff = Pos.Positioned Brick

type Carets = Pos.Positioned ()

data CaretMove
    = MoveVert Int
    | MoveHoriz Int
    deriving (Eq, Show)

data Carpenter a = Carpenter
    { struct  :: Struct
    , builder :: a -> Maybe (Brick, Carpenter a)
    , updater :: Instrs -> (Maybe a, BrickDiff, Carpenter a)
    , rail    :: Carets -> Carets
    }

railTop :: Carpenter a -> Carets -> Carets
railTop cp cs = Pos.onRoots (Pos.roots cs) <> rail cp cs

instance IsoFunctor Carpenter where
    iso <$> cp = Carpenter
        { struct  = struct cp
        , builder = \b -> do
                a <- unapply iso b
                (x, cp') <- builder cp a
                return (x, iso <$> cp')
        , updater = \is ->
                let (ma, bd, cp') = updater cp is
                in  (apply iso =<< ma, bd, iso <$> cp')
        , rail = rail cp
        }

instance ProductFunctor Carpenter where
    (<*>) = flip fix Nothing $ \next mab cp cq -> Carpenter
        { struct  = Struct (size (struct cp) + size (struct cq)) False
        , builder = \(a, b) -> do
                (bd, cp') <- builder cp a
                (be, cq') <- builder cq b
                return (consBrick bd be, next (Just (a, b)) cp' cq')
        , updater = \is ->
                if Pos.null is then (mab, mempty, next mab cp cq)
                else let lsingle       = single $ struct cp
                         rsingle       = single $ struct cq
                         (lis, ris)    = Pos.split (size $ struct cp) is
                         lis'          = applyIf lsingle Pos.head lis
                         ris'          = applyIf rsingle Pos.head ris
                         (ma, bd, cp') = updater cp lis'
                         (mb, be, cq') = updater cq ris'
                         mab'          = zipMaybe ma mb
                         bd'           = applyIf lsingle
                                         Pos.singleton bd
                         be'           = applyIf rsingle
                                         Pos.singleton be
                     in  ( mab'
                         , mergeBrickDiff (size $ struct cp) bd' be'
                         , next mab' cp' cq'
                         )
        , rail = \cs ->
                if Pos.null cs then cs
                else let lsingle    = single $ struct cp
                         rsingle    = single $ struct cq
                         (lcs, rcs) = Pos.split (size $ struct cp) cs
                         singleton cs = applyIf (not $ Pos.null cs)
                                         Pos.singleton cs
                         lcs'       = applyIf lsingle singleton
                                      $ rail cp
                                      $ applyIf lsingle Pos.head
                                      $ Pos.narrow 0 (size (struct cp) - 1) lcs
                         rcs'       = applyIf rsingle singleton
                                      $ rail cq
                                      $ applyIf rsingle Pos.head
                                      $ Pos.narrow 0 (size (struct cq) - 1) rcs
                     in Pos.merge (size $ struct cp) lcs' rcs'
        }

instance Alter Carpenter where
    empty = Carpenter (Struct 0 False)
        (const Nothing)
        (const (Nothing, mempty, empty))
        id
    -- cp@(Carpenter n bf uf) <|> cq@(Carpenter m bg ug)
    (<|>) = flip fix (Nothing, Left ()) $ \next (mab, lr) cp cq -> Carpenter
        { struct  = either (const $ struct cp) (const $ struct cq) lr
        , builder = \a -> case (builder cp a, builder cq a) of
                (Just (ba, cp'), _      ) ->
                    Just (ba, next (Just a, Left ())  cp' cq )
                (_,       Just (bb, cq')) ->
                    Just (bb, next (Just a, Right ()) cp  cq')
                (Nothing, Nothing       ) -> Nothing
        , updater = \is ->
                if Pos.null is then (mab, mempty, next (mab, lr) cp cq)
                else let (ma, bd, cp') = updater cp is
                         (mb, be, cq') = updater cq is
                         (mab', lr')   = case (ma, mb) of
                             (Just _, _) -> (ma, Left ())
                             (_, Just _) -> (mb, Right ())
                             _           -> (Nothing, lr)
                     in  (mab', bd <> be, next (mab', lr') cp' cq')
        , rail = \cs -> if Pos.null cs then cs
                        else either (const $ rail cp) (const $ rail cq) lr cs
        }

instance Structural Carpenter where
    pure a = Carpenter
        { struct  = Struct 0 False
        , builder = \a' -> if a == a' then Just (Empty, pure a) else Nothing
        , updater = const (Just a, mempty, pure a)
        , rail    = id
        }
    char = flip fix Nothing $ \next mc -> Carpenter
        { struct  = Struct 1 True
        , builder = \c -> Just (Plane c, next $ Just c)
        , updater = \is -> case changes is of
                NoChange -> (mc, mempty, next mc)
                Set c    -> (Just c,  Pos.onRoot (Plane c), next $ Just c)
                Delete   -> (Nothing, mempty, next Nothing)
        , rail = Pos.bottom
        }
    sub l = flip fix Nothing $ \next ma cp -> Carpenter
        { struct  = Struct 1 True
        , builder = \a -> do
                (br, cp') <- builder cp a
                return (br, next (Just a) cp')
        , updater = \is ->
                case changes is of
                    NoChange ->
                        let sis = applyIf (single $ struct cp) Pos.head is
                            (ma', bd, cp') = updater cp sis
                        in  (ma', bd, next ma' cp')
                    Set a    -> fromMaybe (ma, mempty, next ma cp) $ do
                        (br, cp') <- builder cp a
                        return (Just a, Pos.onRoot br, next (Just a) cp')
                    Delete   -> (Nothing, mempty, next Nothing cp)
        , rail = \cs ->
                Pos.onRoots (Pos.roots cs)
                <> applyIf (single $ struct cp) Pos.singleton
                    (rail cp $ applyIf (single $ struct cp) Pos.head cs)
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

mergeBrickDiff :: Int -> BrickDiff -> BrickDiff -> BrickDiff
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
