{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Data.Struere.Editor.Carpenter where


import           Control.Applicative      (Const (..))
import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Prelude                  hiding (id, pure, (<$>), (<*>))
-- import           Data.Number.Nat
import           Data.Dynamic
import           Debug.Trace

import           Data.Struere.Isomorphism
import qualified Data.Struere.Position    as Pos
import           Data.Struere.Struct      as St
import           Data.Struere.Syntax      hiding (not)
import           Data.Struere.Util
-- import           Data.Struere.Viewer

-- data Struct = Struct
--     { size   :: Int
--     , single :: Bool
--     }

-- Carpenter

data Instr = INone
           | IDelete
           | ISet Dynamic
           | IInsert Dynamic
           deriving (Show)

type Instrs = Distr Instr

instance Semigroup Instr where
    INone <> x     = x
    x     <> _     = x

instance Monoid Instr where
    mempty = INone

noInstr :: Instr -> Bool
noInstr INone = True
noInstr _     = False

-- type Bubbles = Pos.Positioned Bubble

-- data Bubble = Unaccepted
--             | BDelete



data CaretMove
    = MoveVert Int
    | MoveHoriz Int
    deriving (Eq, Show)

-- data Carpenter a = Carpenter
--     { builder :: Struct -> a -> Maybe (Carpenter a)
--     , updater :: Instrs -> (Maybe (Struct, a), Carpenter a)
--     -- , rail    :: Pos.Carets -> Pos.Carets
    -- }


-- newtype Builder a = Builder
--     { build :: forall f. Syntax f => Maybe a -> Equivalent f Updater a }

-- newtype Updater a = Updater
--     { update :: Diff (Const Instr a) -> Equivalent Diff Updater a }

-- builder :: forall a. (forall f. Syntax f => f a) -> a -> Updater a
-- builder p = _ $ runEquivalent (\b r -> build b r) (p :: Equivalent Builder Radiographer a)

-- instance IsoFunctor Builder where
--     iso <$> bp = Builder $ \ma ->
--         iso <$> build bp (ma >>= unapply iso)

-- instance ProductFunctor Builder where
--     bp <*> bq = Builder $ \mab ->
--         let (ma, mb) = unzipMaybe mab
--         in  build bp ma <*> build bq mb

-- instance Alter Builder where
--     empty = Builder $ \_ -> end empty up
--       where
--         up = Updater $ const (end Empty up)

newtype Builder a = Builder
    { build :: Maybe a -> Updater a }

newtype Updater a = Updater
    { update :: Instrs -> (Maybe a, Updater a) }


builder :: forall a. (forall f. Syntax f => f a) -> a -> Maybe (Updater a)
builder p a = do
    st <- xray p a
    let bp = mono (p :: Poly Syntax Builder a)
    return $ build bp (Just a)

instance IsoFunctor (Poly Syntax Builder) where
    iso <$> Poly p bp = Poly (iso <$> p) $
        Builder $ \mb ->
            let ma  = mb >>= unapply iso
                iua = build bp ma
            in ub iua
      where
        ub ua = Updater $ \is ->
            let (ma, ua') = update ua is
                mb       = ma >>= apply iso
            in  (mb, ub ua')



instance ProductFunctor (Poly Syntax Builder) where
    Poly p bp <*> Poly q bq = Poly (p <*> q) $
        Builder $ \mab ->
            let (ma, mb)     = unzipMaybe mab
                iup = build bp ma
                iuq = build bq mb
            in  upq mab iup iuq
      where
        upq mab up uq = Updater $ \(Distr x di) ->
            fromMaybe (mab, upq mab up uq) $ do
            -- guard $ not $ Pos.null is
            ab <- mab
            let (lis, ris) = deprod di
                (ma,  up') = update up lis
                (mb,  uq') = update uq ris
                mab'       = zipMaybe ma mb
            return (mab', upq mab' up' uq')


instance Alter (Poly Syntax Builder) where
    empty = Poly empty $ Builder $ \_ -> up
      where
        up = Updater $ const (Nothing, up)
    ~(Poly p bp) <|> ~(Poly q bq) = Poly (p <|> q) $
        Builder $ \ ma ->
            upq ma ma (build bp ma) (build bq ma)
      where
        upq ma mb up uq = Updater $ \is ->
            fromMaybe (ma, upq ma mb up uq) $ do
                -- guard $ not $ Pos.null is
                let (ma', up') = maybe (ma, up) (const $ update up is) ma
                    (mb', uq') = maybe (mb, uq) (const $ update uq is) mb
                return (ma' `mplus` mb', upq ma' mb' up' uq')


instance Syntax (Poly Syntax Builder) where
    pure a = Poly (pure a) $ Builder $ \ma ->
        let ma' = if Just a == ma then ma else Nothing
        in  up ma'
      where
        up mt = Updater $ const (mt, up mt)

    char = Poly char $ Builder $ \mc -> up mc
      where
        up mc = Updater $ \(Distr x _) -> case x of
            ISet d  -> maybe (mc, up mc) (\c -> (Just c, up (Just c)))
                       (fromDynamic d)
            IDelete -> (Nothing, up Nothing)
            _       -> (mc, up mc)

    part d (Poly p bp) = Poly (part d p) $
        Builder $ \ma ->
            let iup = build bp ma
            in  upp p ma iup
      where
        upp :: forall a. Typeable a => (forall f. Syntax f => f a)
            -> Maybe a -> Updater a -> Updater a
        upp p ma up = Updater $ \(Distr x di) ->
            case x of
                ISet d -> fromMaybe (ma, upp p ma up) $ do
                    -- up' <- builder p a
                    a <- fromDynamic d
                    let bp  = p :: Poly Syntax Builder a
                        up' = build (mono bp) (Just a)
                    return (Just a, upp p (Just a) up')
                IDelete -> (Nothing, upp p Nothing up)
                _ ->
                    let (ma', up') = update up (desub di)
                    in  (ma', upp p ma' up')





-- instance IsoFunctor Carpenter where
--     iso <$> cp = Carpenter
--         { builder = \b -> do
--                 a <- unapply iso b
--                 (x, cp') <- builder cp a
--                 return (x, iso <$> cp')
--         , updater = \is ->
--                 let (ma, cp') = updater cp is
--                     ma' =
--                         ma >>= apply iso
--                 in  (ma', iso <$> cp')
--         , rail = rail cp
--         }

-- instance ProductFunctor Carpenter where
--     (<*>) = flip fix Nothing $ \next mab cp cq -> Carpenter
--         { builder = \(a, b) -> do
--                 (bd, cp') <- builder cp a
--                 (be, cq') <- builder cq b
--                 return (consBrick bd be, next (Just (a, b)) cp' cq')
--         , updater = \is ->
--                 if Pos.null is then (mab, next mab cp cq)
--                 else let lsingle   = True -- single $ struct cp
--                          rsingle   = True -- single $ struct cq
--                          (lis, ris)= Pos.split 0 is
--                          lis'      = applyIf lsingle Pos.head lis
--                          ris'      = applyIf rsingle Pos.head ris
--                          (ma, cp') = updater cp lis'
--                          (mb, cq') = updater cq ris'
--                          mab'      = zipMaybe ma mb
--                      in  ( mab'
--                          , next mab' cp' cq'
--                          )
--         , rail = \cs ->
--                 if Pos.null cs then cs
--                 else let lsingle    = single $ struct cp
--                          rsingle    = single $ struct cq
--                          (lcs, rcs) = Pos.split (size $ struct cp) cs
--                          singleton cs = applyIf (not $ Pos.null cs)
--                                          Pos.singleton cs
--                          lcs'       = applyIf lsingle singleton
--                                       $ rail cp
--                                       $ applyIf lsingle Pos.head
--                                       $ Pos.narrow 0 (size (struct cp) - 1) lcs
--                          rcs'       = applyIf rsingle singleton
--                                       $ rail cq
--                                       $ applyIf rsingle Pos.head
--                                       $ Pos.narrow 0 (size (struct cq) - 1) rcs
--                      in Pos.merge (size $ struct cp) lcs' rcs'
--         }

-- instance Alter Carpenter where
--     empty = Carpenter (Struct 0 False)
--         (const Nothing)
--         (const (Nothing, mempty, empty))
--         id
--     -- cp@(Carpenter n bf uf) <|> cq@(Carpenter m bg ug)
--     (<|>) = flip fix (Nothing, Left ()) $ \next (mab, lr) cp cq -> Carpenter
--         { builder = \a -> case (builder cp a, builder cq a) of
--                 (Just (ba, cp'), _      ) ->
--                     Just (ba, next (Just a, Left ())  cp' cq )
--                 (_,       Just (bb, cq')) ->
--                     Just (bb, next (Just a, Right ()) cp  cq')
--                 (Nothing, Nothing       ) -> trace "FAIL" Nothing
--         , updater = \is ->
--                 if Pos.null is then (mab, mempty, next (mab, lr) cp cq)
--                 else let (ma, bd, cp') = updater cp is
--                          (mb, be, cq') = updater cq is
--                          (mab', lr')   = case (ma, mb) of
--                              (Just _, _) -> (ma, Left ())
--                              (_, Just _) -> (mb, Right ())
--                              _           -> (Nothing, lr)
--                      in  (mab', bd <> be, next (mab', lr') cp' cq')
--         , rail = \cs -> if Pos.null cs then cs
--                         else either (const $ rail cp) (const $ rail cq) lr cs
--         }

-- instance Syntax Carpenter where
--     pure a = Carpenter
--         { builder = \a' -> if a == a' then Just (Empty, pure a) else Nothing
--         , updater = const (Just a, mempty, pure a)
--         , rail    = id
--         }
--     char = flip fix Nothing $ \next mc -> Carpenter
--         { builder = \c -> Just (Token c, next $ Just c)
--         , updater = \is -> case changes is of
--                 NoChange -> (mc, mempty, next mc)
--                 Set c    -> (Just c,  Pos.onRoot (Token c), next $ Just c)
--                 Delete   -> (Nothing, mempty, next Nothing)
--         , rail = Pos.bottom
--         }
--     part d = flip fix Nothing $ \next ma cp -> Carpenter
--         { builder = \a -> do
--                 (br, cp') <- builder cp a
--                 return (br, next (Just a) cp')
--         , updater = \is ->
--                 case changes is of
--                     NoChange ->
--                         let sis = applyIf (single $ struct cp) Pos.head is
--                             (ma', bd, cp') = updater cp sis
--                         in  (ma', bd, next ma' cp')
--                     Set a    -> fromMaybe (ma, mempty, next ma cp) $ do
--                         (br, cp') <- builder cp a
--                         return (Just a, Pos.onRoot br, next (Just a) cp')
--                     Delete   -> (Nothing, mempty, next Nothing cp)
--         , rail = \cs ->
--                 Pos.onRoots (Pos.roots cs)
--                 <> applyIf (single $ struct cp) Pos.singleton
--                     (rail cp $ applyIf (single $ struct cp) Pos.head cs)
--         }

-- data Changes a = NoChange
--                | Set a
--                | Delete
--                deriving (Show)

-- changes :: Typeable a => Instrs -> Changes a
-- changes is = case Pos.roots is of
--     IDelete : _ -> Delete
--     -- ISet d  : _ -> maybe NoChange Set $ fromDynamic d
--     ISet d  : _ -> maybe NoChange Set $ fromDynamic d
--     _           -> NoChange

-- mergeBrickDiff :: Int -> BrickDiff -> BrickDiff -> BrickDiff
-- mergeBrickDiff n [Root (Token xs)] [Root (Token ys)] = [Root $ Token $ xs ++ ys]
-- mergeBrickDiff n x y                                 = Pos.merge n x y

-- Utilities

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


-- instance Syntax Builder where
--     char = Builder $ \c ->
--         let sc = Carpenter 1 $ \is _ ->
--                 case fst $ unconsRoot is of
--                     IChar c'   : _ -> Just (c', [], [Root $ Token [c']], sc)
--                     IDelete    : _ -> Nothing
--                     IReplace d : _ -> Nothing
--                     _              -> Just (c, [], [], sc)
--         in  Just sc
--     pure a = Builder $ \_ ->
--         let sc = Carpenter 1 $ \_ _ -> Just (a, [], [], sc)
--         in  Just sc

-- runBuilder :: Builder a -> a -> Maybe (Carpenter a)
-- runBuilder (Builder f) = f
