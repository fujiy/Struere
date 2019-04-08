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
           | IInsert (Fragment Scaffold)
           deriving (Show)

type Instrs = Distr Instr

instance Show (Fragment Scaffold) where
    show _ = "<<fragment>>"

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
            | BReplace (Fragment Updater) (Fragment Scaffold)
            | BInsert (Fragment Scaffold)
            -- | BInsertRec (Fragment Updater) (Fragment Scaffold)

instance Show Bubble where
    show BNone          = "BNone"
    show BDelete        = "BDelete"
    show (BReplace _ _) = "BReplace"
    show (BInsert _)    = "BInsert"

instance Semigroup Bubble where
    BNone <> x     = x
    x     <> _     = x

instance Monoid Bubble where
    mempty = BNone


data Builder a = Builder
    { build :: Blueprint -> Scaffold a -> Updater a
    , fill  :: Blueprint -> Fragment Scaffold -> Maybe (Scaffold a, Updater a)
    }

newtype Updater a = Updater
    { update :: Instrs -> (Bubble, Scaffold a, Updater a) }


builder :: forall a. Blueprint -> (forall f. Syntax f => f a) -> a
        -> Maybe (Updater a)
builder bp p a = do
    st <- xray p a
    sa <- scaffolder p bp a
    let bl = mono (p :: Poly Syntax Builder a)
    return $ build bl bp sa

instance IsoFunctor (Poly Syntax Builder) where
    iso <$> Poly p bl = Poly (iso <$> p) $
        fix $ \fbl -> Builder
        { build = \(BIsoMap u bp) (decoerce u -> Just sb) ->
                up u $ build bl bp sb
        , fill = filler fbl $
            \(BIsoMap u bp) fs -> do
                (sa, ua) <- fill bl bp fs
                return (coerceSC u iso sa, up u ua)
        }
      where
        up u ua = Updater $ \is ->
            let (b, sa, ua') = update ua is
                sb       = coerceSC u iso sa
            in replaceByBubble u (b, sb, up u ua')


instance ProductFunctor (Poly Syntax Builder) where
    Poly p bl <*> Poly q br = Poly (p <*> q) $
        fix $ \fbl -> Builder
        { build = \b sab ->
                let ~(BProduct u bp bq) = b
                    (sa, sb) = depair sab
                    iup      = build bl bp sa
                    iuq      = build br bq sb
                in  upq b sab iup iuq
        , fill = filler fbl $
            \b@(BProduct u bp bq) fs ->
                (do  (sa, up) <- fill bl bp fs
                     let sb  = novalue $ unique bq
                         sab = pairSC u sa sb
                         uq  = build br bq sb
                     return (sab, upq b sab up uq))
                -- `mplus`
                -- (do  (sb, uq) <- fill br bq fs
                --      let sa  = novalue $ unique bp
                --          sab = pairSC u sa sb
                --          up  = build bl bp sa
                --      return (sab, upq b sab up uq))

        }
      where
        upq b sab up uq = Updater $ \(Distr x di) ->
            fromMaybe (traceShow "NO" (mempty, sab, upq b sab up uq)) $ do
            -- guard $ not $ Pos.null is
            let ~(BProduct u bp bq) = b
                (lis, ris)    = deprod di
                (bx, sa, up') = update up lis
                (by, sb, uq') = update uq ris
                sab'          = pairSC u sa sb
                fup           = toFrag (unique bp) up'
                fuq           = toFrag (unique bq) uq'
                upq'          = upq b sab' up' uq'
                fupq          = toFrag u upq'
                fsa           = toFrag (unique bp) sa
                fsb           = toFrag (unique bq) sb
            return . replaceByBubble u $ case (bx, by) of
                (BDelete, _) ->
                    (BReplace fuq fsb, sab, upq b sab up' uq')
                (_, BDelete) ->
                    (BReplace fup fsa, sab, upq b sab up' uq')
                (BInsert fa, _) -> fromMaybe (mempty, sab', upq') $ do
                    (sa', up'') <- fill bl bp fa
                    (sb', uq'') <- fill br bq $ toFrag u sab'
                    let sab'' = pairSC u sa' sb'
                        upq'' = upq b sab'' up'' uq''
                    return (mempty, sab'', upq'')
                _   -> (bx <> by, sab', upq')

instance Alter (Poly Syntax Builder) where
    empty = Poly empty $ Builder
        { build = \_ sa -> up sa
        , fill  = \_ _ -> Nothing
        }
      where
        up sa = Updater $ const (mempty, sa, up sa)

    ~(Poly p bl) <|> ~(Poly q br) = Poly (p <|> q)
        $ fix $ \fbl -> Builder
        { build = \(~(BAlter u bp bq)) se ->
                case extract se of
                    Left  sa -> upq u (Left  sa) (build bl bp sa)
                    Right sa -> upq u (Right sa) (build br bq sa)
        , fill = filler fbl $ \(BAlter u bp bq) fs ->
                (do  (sa, up) <- fill bl bp fs
                     return (inL u sa, upq u (Left sa) up))
                `mplus`
                (do  (sa, uq) <- fill br bq fs
                     return (inR u sa, upq u (Right sa) uq))
        }
      where
        upq u es up = Updater $ \is ->
            replaceByBubble u $ case es of
            Left  sa -> let (ba, sa', up') = update up is
                        in  (ba, inL u sa', upq u (Left sa') up')
            Right sa -> let (ba, sa', up') = update up is
                        in  (ba, inR u sa', upq u (Right sa') up')

instance Syntax (Poly Syntax Builder) where
    pure a = Poly (pure a) Builder
        { build = \_ sa -> up sa
        , fill = \b _ -> let sa = Scaffold (unique b) (Just a) Edge
                         in  return (sa, up sa)
        }
      where
        up sa = Updater $ const (mempty, sa, up sa)

    char = Poly char $ fix $ \fbl -> Builder
        { build = up
        , fill = filler fbl $ \_ _ -> Nothing
        }
      where
        up b sc = Updater $ \(Distr x _) -> case x of
            ISet d  -> maybe (traceShow "NO" (mempty, sc, up b sc))
                       (\c -> let sc' = Scaffold (unique b) c Edge
                              in (mempty, sc', up b sc'))
                       (fromDynamic d)
            IDelete -> (BDelete, sc, up b sc)
            IInsert fa -> (BInsert fa, sc, up b sc)
            _       -> (mempty, sc, up b sc)

    part d (Poly p bl) = Poly (part d p) $ fix $ \fbl -> Builder
        { build = \bp sa ->
            let ~(BNest u bq) = bp
                iup = build bl bq (desingle sa)
            in  upp u bp sa iup
        , fill = filler fbl $ \b@(BNest u bp) fs -> do
                (sa, up) <- fill bl bp fs
                let sb = Scaffold u (value sa) (Single sa)
                return (sb, upp u b sb up)
        }
      where
        upp u bp sa up = Updater $ \(Distr x di) ->
            replaceByBubble u $ case x of
                -- ISet d -> fromMaybe (mempty, sa, upp u bp sa up) $ do
                --     a <- fromDynamic d
                --     let
                --         -- bl  = p :: Poly Syntax Builder a
                --         up' = build bl bp (Just a)
                --     return (mempty, Just a, upp u bp (Just a) up')
                IDelete    -> (BDelete, sa, upp u bp sa up)
                -- IInsert fa -> (BInsert fa, sa, upp u bp sa up)
                _ ->
                    let (bx, sa', up') = update up (desub di)
                    in  (bx, sa', upp u bp sa' up')


replace :: Unique -> Bubble -> Maybe (Updater a, Scaffold a)
replace u (BReplace fup fma) = zipMaybe (fromFrag u fup) (fromFrag u fma)
replace u _                  = Nothing

replaceByBubble :: Unique -> (Bubble, Scaffold a, Updater a)
                -> (Bubble, Scaffold a, Updater a)
replaceByBubble u (replace u -> Just (up, sa), _, _) = (mempty, sa, up)
replaceByBubble u r                                  = r

filler :: Builder a
       -> (Blueprint -> Fragment Scaffold -> Maybe (Scaffold a, Updater a))
       -> Blueprint -> Fragment Scaffold -> Maybe (Scaffold a, Updater a)
filler bl up b fs =
    case fromFrag (unique b) fs of
        Nothing -> up b fs
        Just sa -> Just (sa, build bl b sa)




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
