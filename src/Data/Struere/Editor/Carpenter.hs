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
import           Data.Either
import           Data.Function
import qualified Data.IntSet              as Set
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


instance Semigroup Instr where
    INone <> x     = x
    x     <> _     = x

instance Monoid Instr where
    mempty = INone

noInstr :: Instr -> Bool
noInstr INone = True
noInstr _     = False


data Fill = FNone
          | FOne  (Fragment Scaffold)
          | FCons (Fragment Scaffold) (Fragment Scaffold)


data Bubble = BNone
            | BDelete
            | BReplace [Fragment Scaffold]
            | BInsert  (Fragment Scaffold)
            | BAppend  (Fragment Scaffold) (Fragment Scaffold)
            | BReject Instr

instance Show Bubble where
    show BNone          = "BNone"
    show BDelete        = "BDelete"
    show (BReplace fss) = "BReplace" ++ show fss
    show (BInsert _)    = "BInsert"
    show (BReject _)    = "BReject"

instance Semigroup Bubble where
    BNone <> x     = x
    x     <> _     = x

instance Monoid Bubble where
    mempty = BNone


type Tested = Set.IntSet


newtype Builder a = Builder
    { build :: Blueprint -> Scaffold a -> Updater a }

data Updater a = Updater
    { update :: Instrs -> (Bubble, Scaffold a, Updater a)
    , fill   :: Tested -> Fill -> Maybe (Scaffold a, Updater a)}


builder :: forall a. Blueprint -> (forall f. Syntax f => f a) -> a
        -> Maybe (Updater a)
builder bp p a = do
    st <- xray p a
    sa <- scaffolder p bp a
    let bl = mono (p :: Poly Syntax Builder a)
    return $ build bl bp sa

instance IsoFunctor (Poly Syntax Builder) where
    iso <$> Poly p bl = Poly (iso <$> p) $
        fix $ \fbl -> Builder $
        \blp@(BIsoMap u bp) sb ->
            let sa = fromMaybe (novalue $ unique bp) $ decoerce u sb
            in  up blp fbl sb $ build bl bp sa
      where
        up blp@(BIsoMap u bp) fbl sb ua = Updater
            { update = \is ->
                    let (b, sa, ua')    = update ua is
                        (accepted, sb') = coerceSC u iso sa
                        b'              = applyUnless accepted
                                          ((BReject $ flatten' is) <>) b
                        sb''            = if accepted then sb' else sb
                        ua''            = if accepted then ua' else ua
                    in  replaceByBubble fbl blp u
                        (b', sb'', up blp fbl sb'' ua'')
            , fill = filler fbl blp $ \ts fs -> traceShow ('I', u) $ do
                    (sa, ua) <- fill ua ts fs
                    let (accepted, sb) = coerceSC u iso sa
                    if accepted then return (sb, up blp fbl sb ua) else Nothing
            }



instance ProductFunctor (Poly Syntax Builder) where
    Poly p bl <*> Poly q br = Poly (p <*> q) $
        fix $ \fbl -> Builder $ \blp sab ->
        let ~(BProduct u bp bq) = blp
            (sa, sb) = fromMaybe (novalue $ unique bp, novalue $ unique bq)
                       $ depair' sab
            iup      = build bl bp sa
            iuq      = build br bq sb
        in  upq blp fbl sab iup iuq
              where
        upq blp@(BProduct u bp bq) fbl sab up uq = Updater
            { update =
              \(Distr x di) ->
                  fromMaybe (mempty, sab, upq blp fbl sab up uq) $ do
                  let (lis, ris)    = deprod di
                      (bx, sa, up') = update up lis
                      (by, sb, uq') = update uq ris
                      sab'          = pairSC u sa sb
                      upq'          = upq blp fbl sab' up' uq'
                      fup           = toFrag (unique bp) up'
                      fuq           = toFrag (unique bq) uq'
                      fsa           = toFrag (unique bp) sa
                      fsb           = toFrag (unique bq) sb
                  return . replaceByBubble fbl blp u $ case (bx, by) of
                      (BDelete, _) -> traceShow ("dl", u, unique bq)
                          (BReplace [fsb], sab, upq blp fbl sab up uq')
                      -- (_, BDelete) -> traceShow ("dr", u, unique bp)
                      --     (BReplace fup fsa, sab, upq blp fbl sab up' uq)
                      (BReplace fss, _) ->
                          (BReplace (fsb:fss), sab, upq blp fbl sab up uq')
                      (BInsert fa, _) ->
                          fromMaybe (bx, sab, upq blp fbl sab up uq') $ do
                          (sa', up'') <- fill up mempty $ FOne fa
                          (sb', uq'') <- fill uq mempty $ FOne
                                         $ toFrag u sab'
                          let sab'' = pairSC u sa' sb'
                              upq'' = upq blp fbl sab'' up'' uq''
                          return (mempty, sab'', upq'')
                      _   -> (bx <> by, sab', upq')
            , fill = filler fbl blp $ \ts fl -> traceShow ("P", u) $
                    let fill2 fla flb = do
                            (sa, up) <- traceShow "L" $ fill up ts fla
                            (sb, uq) <- traceShow "R" $ fill uq ts flb
                            let sab = pairSC u sa sb
                            return (sab, upq blp fbl sab up uq)
                    in case fl of
                        FNone       -> fill2 FNone FNone
                        FOne fs     -> fill2 fl FNone `mplus` fill2 FNone fl
                        FCons fa fb -> fill2 (FOne fa) (FOne fb) `mplus`
                                       fill2 fl FNone `mplus` fill2 FNone fl
        }



instance Alter (Poly Syntax Builder) where
    empty = Poly empty $ Builder $ \_ sa -> up sa
      where
        up sa = Updater
            { update = const (mempty, sa, up sa)
            , fill  = \_ _ -> Nothing
            }

    ~(Poly p bl) <|> ~(Poly q br) = Poly (p <|> q)
        $ fix $ \fbl -> Builder $
        \blp@(~(BAlter u bp bq)) se ->
            case fromMaybe (Right $ novalue $ unique bq) $ extract' se of
                Left  sa -> upq blp fbl (Left  sa)
                            (build bl bp sa)
                            (build br bq $ novalue (unique bq))
                Right sa -> upq blp fbl (Right sa)
                            (build bl bp $ novalue (unique bp))
                            (build br bq sa)
      where
        upq blp@(BAlter u bp bq) fbl es up uq = Updater
            { update = \is -> replaceByBubble fbl blp u $
                let (ba', saa, upp) = case es of
                        Left  sa -> let (ba, sa', up') = update up is
                                    in  ( ba, inL u sa'
                                        , upq blp fbl (Left sa') up' uq)
                        Right sa -> let (ba, sa', uq') = update uq is
                                    in  ( ba, inR u sa'
                                        , upq blp fbl (Right sa') up uq')
                in case ba' of
                    BInsert fa -> traceShow ('I',u, ba') $
                                  fromMaybe (ba', saa, upp) $ case es of
                        -- Left sa ->
                        --     let fb = toFrag (unique bp) sa
                        --     in do (sa', up') <- fill up mempty $ FCons fa fb
                        --           return ( mempty, inR u sa'
                        --                  , upq blp fbl (Right sa') up' )
                        Right sa -> traceShow ("RIGHT", up `seq` u) $
                            let fb = toFrag (unique bq) sa
                            in do (sa', up') <- fill up mempty $ FCons fa fb
                                  return ( mempty, inL u sa'
                                         , upq blp fbl (Left sa') up' uq)
                        _ -> traceShow "LEFT" Nothing
                    _ -> (ba', saa, upp)
            , fill = filler fbl blp $ \ts fs ->
                    (do (sa, up') <- fill up ts fs
                        return (inL u sa, upq blp fbl (Left sa)  up' uq))
                    `mplus`
                    (do (sa, uq') <- fill uq ts fs
                        return (inR u sa, upq blp fbl (Right sa) up uq'))
            }


instance Syntax (Poly Syntax Builder) where
    pure a = Poly (pure a) $ Builder $ \blp sa -> up blp sa
      where
        up blp sa = Updater
            { update = \(Distr x _) -> case x of
                    IInsert fa -> (BInsert fa, sa, up blp sa)
                    _          -> (mempty, sa, up blp sa)
            , fill = \_ fbl -> case fbl of
                    FNone -> let sa = edgeSC (unique blp) a
                             in  return (sa, up blp sa)
                    FOne fs -> do
                        sa <- fromFrag (unique blp) fs
                        if value sa == Just a
                            then return (sa, up blp sa)
                            else Nothing
                    _ -> Nothing
            }


    char = Poly char $ fix $ \fbl -> Builder $ \blp sc -> up blp fbl sc
      where
        up blp fbl sc = Updater
            { update = \(Distr x _) -> case x of
                    ISet d  -> maybe (traceShow "NO" (mempty, sc, up blp fbl sc))
                               (\c -> let sc' = Scaffold (unique blp) c Edge
                                      in (mempty, sc', up blp fbl sc'))
                               (fromDynamic d)
                    IDelete -> (BDelete, sc, up blp fbl sc)
                    IInsert fa -> (BInsert fa, sc, up blp fbl sc)
                    _       -> (mempty, sc, up blp fbl sc)
            , fill = filler fbl blp $ \_ _ -> Nothing
            }

    part d (Poly p bl) = Poly (part d p) $
        fix $ \fbl -> Builder $ \blp sa ->
        let ~(BNest u bq) = blp
            iup = build bl bq (fromMaybe (novalue $ unique bq) $ desingle' sa)
        in  upp blp fbl sa iup
      where
        upp blp@(BNest u bp) fbl sa up = Updater
            { update = \(Distr x di) ->
                    replaceByBubble fbl blp u $ case x of
                -- ISet d -> fromMaybe (mempty, sa, upp u bp sa up) $ do
                --     a <- fromDynamic d
                --     let
                --         -- bl  = p :: Poly Syntax Builder a
                --         up' = build bl bp (Just a)
                --     return (mempty, Just a, upp u bp (Just a) up')
                    IDelete    -> (BDelete, sa, upp blp fbl sa up)
                    IInsert fa -> (BInsert fa, sa, upp blp fbl sa up)
                        -- fromMaybe (BInsert fa, sa, upp blp fbl sa up) $ do
                        -- (sa', up') <- traceShow u $ fill up mempty $ FOne fa
                        -- let sb = Scaffold u (value sa') (Single sa')
                        -- return (mempty, sb, upp blp fbl sa' up')
                    _ ->
                        let (bx, sa', up') = update up (desub di)
                            sb             = Scaffold u (value sa') (Single sa')
                        in  (bx, sb, upp blp fbl sa' up')
            , fill = filler fbl blp $ \ts fs -> do
                    (sa, up) <- fill up ts fs
                    let sb = Scaffold u (value sa) (Single sa)
                    return (sb, upp blp fbl sb up)
        }



replace :: Unique -> Bubble -> Maybe (Scaffold a)
replace u (BReplace fss) = listToMaybe . catMaybes $ map (fromFrag u) fss
replace u _              = Nothing

replaceByBubble :: Builder a -> Blueprint -> Unique
                -> (Bubble, Scaffold a, Updater a)
                -> (Bubble, Scaffold a, Updater a)
replaceByBubble bl bp u (replace u -> Just sa, _, ua) =
    (mempty, sa, build bl bp sa)
replaceByBubble bl bp u (b, sa, ua) = traceShow ('r', u, b) (b, sa, ua)
replaceByBubble bl bp u r           = r

filler :: Builder a -> Blueprint
       -> (Tested -> Fill -> Maybe (Scaffold a, Updater a))
       -> Tested -> Fill -> Maybe (Scaffold a, Updater a)
filler bl b up ts _ | unique b `Set.member` ts = Nothing
filler bl b up ts (FOne fs) =
    case fromFrag (unique b) fs of
        Nothing -> up (Set.insert (unique b) ts) (FOne fs)
        Just sa -> Just (sa, build bl b sa)
filler bl b up ts fl = up (Set.insert (unique b) ts) fl



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
