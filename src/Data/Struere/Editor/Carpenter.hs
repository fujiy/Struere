{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

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


data Bubble = BNone
            | BDelete
            | BReplace (Fragment Updater) (Fragment Maybe)

instance Show Bubble where
    show BNone          = "BNone"
    show BDelete        = "BDelete"
    show (BReplace _ _) = "BReplace"

instance Semigroup Bubble where
    BNone <> x     = x
    x     <> _     = x

instance Monoid Bubble where
    mempty = BNone


newtype Builder a = Builder
    { build :: Blueprint -> Maybe a -> Updater a }

newtype Updater a = Updater
    { update :: Instrs -> (Bubble, Maybe a, Updater a) }


builder :: forall a. Blueprint -> (forall f. Syntax f => f a) -> a
        -> Maybe (Updater a)
builder bp p a = do
    st <- xray p a
    let bl = mono (p :: Poly Syntax Builder a)
    return $ build bl bp (Just a)

instance IsoFunctor (Poly Syntax Builder) where
    iso <$> Poly p bl = Poly (iso <$> p) $
        Builder $ \(~(BIsoMap u bp)) mb ->
            let ma  = mb >>= unapply iso
                iua = build bl bp ma
            in ub u iua
      where
        ub u ua = Updater $ \is ->
            let (b, ma, ua') = update ua is
                mb       = ma >>= apply iso
            in replaceByBubble u (b, mb, ub u ua')



instance ProductFunctor (Poly Syntax Builder) where
    Poly p bl <*> Poly q br = Poly (p <*> q) $
        Builder $ \b mab ->
            let ~(BProduct u bp bq) = b
                (ma, mb) = unzipMaybe mab
                iup      = build bl bp ma
                iuq      = build br bq mb
            in  upq b mab iup iuq
      where
        upq b mab up uq = Updater $ \(Distr x di) ->
            fromMaybe (mempty, mab, upq b mab up uq) $ do
            -- guard $ not $ Pos.null is
            let ~(BProduct u bp bq) = b
                (lis, ris)    = deprod di
                (bx, ma, up') = update up lis
                (by, mb, uq') = update uq ris
                mab'          = zipMaybe ma mb
                fup           = toFrag (unique bp) up'
                fuq           = toFrag (unique bq) uq'
                fma           = toFrag (unique bp) ma
                fmb           = toFrag (unique bq) mb
            return . replaceByBubble u $ case (bx, by) of
                (BDelete, _) ->
                    (BReplace fuq fmb, mab, upq b mab up uq)
                (_, BDelete) ->
                    (BReplace fup fma, mab, upq b mab up uq)
                _   -> (bx <> by, mab', upq b mab' up' uq')

instance Alter (Poly Syntax Builder) where
    empty = Poly empty $ Builder $ \_ _ -> up
      where
        up = Updater $ const (mempty, Nothing, up)

    ~(Poly p bl) <|> ~(Poly q br) = Poly (p <|> q) $
        Builder $ \(~(BAlter u bp bq)) ma ->
            upq u ma ma (build bl bp ma) (build br bq ma)
      where
        upq u ma mb up uq = Updater $ \is ->
            replaceByBubble u .
            fromMaybe (mempty, ma, upq u ma mb up uq) $ do
                -- guard $ not $ Pos.null is
                let (bx, ma', up') = maybe (mempty, ma, up)
                                     (const $ update up is) ma
                    (by, mb', uq') = maybe (mempty, mb, uq)
                                     (const $ update uq is) mb
                return (bx <> by, ma' `mplus` mb', upq u ma' mb' up' uq')


instance Syntax (Poly Syntax Builder) where
    pure a = Poly (pure a) $ Builder $ \_ ma ->
        let ma' = if Just a == ma then ma else Nothing
        in  up ma'
      where
        up mt = Updater $ const (mempty, mt, up mt)

    char = Poly char $ Builder $ \_ mc -> up mc
      where
        up mc = Updater $ \(Distr x _) -> case x of
            ISet d  -> maybe (mempty, mc, up mc)
                       (\c -> (mempty, Just c, up (Just c)))
                       (fromDynamic d)
            IDelete -> (BDelete, Nothing, up Nothing)
            -- IInsert d -> _
            _       -> (mempty, mc, up mc)

    part d (Poly p bl) = Poly (part d p) $
        Builder $ \bp ma ->
            let ~(BNest u bq) = bp
                iup = build bl bq ma
            in  upp u bp ma iup
      where
        upp u bp ma up = Updater $ \(Distr x di) ->
            replaceByBubble u $ case x of
                ISet d -> fromMaybe (mempty, ma, upp u bp ma up) $ do
                    a <- fromDynamic d
                    let
                        -- bl  = p :: Poly Syntax Builder a
                        up' = build bl bp (Just a)
                    return (mempty, Just a, upp u bp (Just a) up')
                IDelete -> (mempty, Nothing, upp u bp Nothing up)
                _ ->
                    let (bx, ma', up') = update up (desub di)
                    in  (bx, ma', upp u bp ma' up')


replace :: Unique -> Bubble -> Maybe (Updater a, Maybe a)
replace u (BReplace fup fma) = zipMaybe (fromFrag u fup) (fromFrag u fma)
replace u _                  = Nothing

replaceByBubble :: Unique -> (Bubble, Maybe a, Updater a)
                -> (Bubble, Maybe a, Updater a)
replaceByBubble u (replace u -> Just (up, ma), _, _) = (mempty, ma, up)
replaceByBubble u r                                  = r





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
