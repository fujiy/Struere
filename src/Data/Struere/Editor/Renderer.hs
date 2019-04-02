{-# LANGUAGE RankNTypes #-}

module Data.Struere.Editor.Renderer where


import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (apply)
-- import           Graphics.UI.Threepenny.Events

import           Data.Struere.Editor.Brick
import           Data.Struere.Isomorphism
import qualified Data.Struere.Position       as Pos
import           Data.Struere.Struct
import           Data.Struere.Syntax
import           Data.Struere.Util

newtype Renderer a = Renderer
    { renderer :: Carets -> a -> Maybe (UI [Element]) }


instance IsoFunctor Renderer where
    iso <$> Renderer f = Renderer
        $ \cs a ->
              unapply iso a >>= f cs
        -- maybeList . fmap (f cs) . unapply iso

instance ProductFunctor Renderer where
    Renderer v <*> Renderer w
        = Renderer $ \(Distr _ dc) (a, b) -> do
            let (ca, cb) = deprod dc
            ua <- v ca a
            ub <- w cb b
            return $ liftM2 (++) ua ub

instance Alter Renderer where
    empty = Renderer $ \cs _ -> Nothing
    Renderer v <|> Renderer w =
        Renderer $ \cs a -> v cs a `mplus` w cs a

instance Syntax Renderer where
    pure a = Renderer $ \cs _ -> Just $ return []
    char   = Renderer $ \(Distr x _) c ->
        Just $ (:[]) `fmap` caret x (UI.span # set UI.text [c])
    part d (Renderer v) = Renderer $ \(Distr x dc) a -> do
        ui <- v (desub dc) a
        return $ do
            es <- ui
            e <- caret x $ UI.new # set UI.children es
            return [e]

    -- part d p = Renderer $ \st cs a -> hoge p a

caret :: Caret -> UI Element -> UI Element
caret c =
    -- unless  $
    applyWhen (c /= NoCaret)
        -- (null $ Pos.roots cs)
        (set UI.style
        [ ("background", "black")
        , ("color",      "white")]
        )
    -- u

render :: (forall f. Syntax f => f a) -> Carets -> a -> Maybe (UI Element)
render p cs a = do
    st <- xray p a
    ui <- renderer p cs a
    return $ ui >>= ((UI.new #) . set UI.children)

-- runViewer :: Renderer a -> a -> Brick
-- runViewer (Renderer f) = Array . f



maybeList :: Maybe [a] -> [a]
maybeList Nothing   = []
maybeList (Just xs) = xs
