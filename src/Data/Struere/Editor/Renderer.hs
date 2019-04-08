{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Data.Struere.Editor.Renderer where


import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Maybe

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (apply, value)
-- import           Graphics.UI.Threepenny.Events

import           Data.Struere.Editor.Brick
import           Data.Struere.Isomorphism
import qualified Data.Struere.Position       as Pos
import           Data.Struere.Struct
import           Data.Struere.Syntax
import           Data.Struere.Util

newtype Renderer a = Renderer
    { renderer :: Blueprint -> Carets -> Scaffold a -> UI [Element] }

instance IsoFunctor Renderer where
    iso <$> Renderer f = Renderer
        $ \(BIsoMap u bp) cs sa ->
         maybe (hole "I") (f bp cs) $ decoerce u sa

        -- maybeList . fmap (f cs) . unapply iso

instance ProductFunctor Renderer where
    Renderer v <*> Renderer w
        = Renderer $ \(BProduct u bp bq) (Distr _ dc) sab ->
        maybe (hole $ "P" ++ show u)
        (\(sa, sb) ->
            let (ca, cb) = deprod dc
                ua = v bp ca sa
                ub = w bq cb sb
            in  liftM2 (++) ua ub
        ) $ depair' sab

instance Alter Renderer where
    empty = Renderer $ \_ cs _ -> hole "E"
    Renderer v <|> Renderer w =
        Renderer $ \(~(BAlter u bp bq)) cs se ->
        maybe (hole "A")
        (\case
            Left  sa -> v bp cs sa
            Right sa -> w bq cs sa)
        $ extract' se

instance Syntax Renderer where
    pure a = Renderer $ \_ (Distr x _) _ ->
        if x /= NoCaret
        then (:[]) `fmap` caret x (UI.span # set UI.html "&ensp;")
        else return []
    char   = Renderer $ \_ (Distr x _) c ->
        maybe (hole "C")
        (\c -> (:[]) `fmap` caret x (UI.span # set UI.text [c]))
        $ value c
    part d (Renderer v) = Renderer $ \(BNest u bp) (Distr x dc) sa ->
        maybe (hole $ "N" ++ show u)
        (\sb -> do
                es <- v bp (desub dc) sb
                e <- caret x $ UI.new # set UI.children es
                return [e]
        )
        $ desingle' sa
    -- part d p = Renderer $ \st cs a -> hoge p a

hole :: String -> UI [Element]
hole s = fmap (:[]) $ UI.span # set UI.text ("_" ++ s ++ "_") # set UI.style
        [ ("background", "gray")
        , ("color",      "white")
        ]


caret :: Caret -> UI Element -> UI Element
caret c =
    -- unless  $
    applyWhen (c /= NoCaret)
        -- (null $ Pos.roots cs)
        (set UI.style
        [ ("background", "black")
        , ("color",      "white")
        ]
        )
    -- u

-- render ::  -> Carets -> a -> Maybe (UI Element)
-- render p cs a = do
--     st <- xray p a
--     ui <- renderer p cs a
--     return $ ui >>= ((UI.new #) . set UI.children)

-- runViewer :: Renderer a -> a -> Brick
-- runViewer (Renderer f) = Array . f



maybeList :: Maybe [a] -> [a]
maybeList Nothing   = []
maybeList (Just xs) = xs
