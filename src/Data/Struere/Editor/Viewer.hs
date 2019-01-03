
module Data.Struere.Editor.Viewer where

import           Control.Monad

import           Data.Struere.Editor.Brick
import           Data.Struere.Isomorphism
import           Data.Struere.Structural

newtype Viewer a = Viewer (a -> Maybe Brick)

instance IsoFunctor Viewer where
    iso <$> Viewer f = Viewer $ unapply iso >=> f

instance ProductFunctor Viewer where
    Viewer v <*> Viewer w
        = Viewer $ \(a, b) -> liftM2 consBrick (v a) (w b)

instance Alter Viewer where
    empty = Viewer $ const Nothing
    Viewer v <|> Viewer w = Viewer $ \a -> v a `mplus` w a

instance Structural Viewer where
    pure a = Viewer $ const $ Just Empty
    char   = Viewer $ Just . Plane

runViewer :: Viewer a -> a -> Maybe Brick
runViewer (Viewer v) = v
