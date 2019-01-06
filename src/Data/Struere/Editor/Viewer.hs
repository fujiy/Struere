
module Data.Struere.Editor.Viewer where

import           Control.Monad

import           Data.Struere.Editor.Brick
import           Data.Struere.Isomorphism
import           Data.Struere.Structural

newtype Viewer a = Viewer (a -> [Brick])

instance IsoFunctor Viewer where
    iso <$> Viewer f = Viewer $ maybeList . fmap f . unapply iso

instance ProductFunctor Viewer where
    Viewer v <*> Viewer w
        = Viewer $ \(a, b) -> v a <> w b

instance Alter Viewer where
    empty = Viewer $ const []
    Viewer v <|> Viewer w = Viewer $ \a -> v a `mplus` w a

instance Structural Viewer where
    pure a = Viewer $ const []
    char   = Viewer $ (:[]) . Plane
    sub l (Viewer v) = Viewer $ \a -> [Array $ v a]

runViewer :: Viewer a -> a -> Brick
runViewer (Viewer f) = Array . f

maybeList :: Maybe [a] -> [a]
maybeList Nothing   = []
maybeList (Just xs) = xs
