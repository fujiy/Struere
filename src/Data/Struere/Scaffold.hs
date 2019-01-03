{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses     #-}

module Data.Struere.Scaffold where

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

class Monad m => Scaffold m a where
    -- update :: a -> m (Maybe a)

data Diff a = NoChanges
            | Updated a
            | forall x. Partial (Diff (x -> a)) (Diff x)

newtype Diff' a = Diff' (a -> a)

instance Functor Diff where
    f `fmap` NoChanges     = NoChanges
    f `fmap` Updated a     = Updated $ f a
    f `fmap` Partial dg db = Partial ((f . ) `fmap` dg) db

instance Applicative Diff where
    pure = Updated
    NoChanges     <*> NoChanges     = NoChanges
    NoChanges     <*> da            = Partial NoChanges da
    df            <*> NoChanges     = Partial df NoChanges
    Updated f     <*> Updated a     = Updated $ f a
    Partial df dx <*> da            = Partial ((flip <$> df) <*> da) dx
    df            <*> Partial dg dx = Partial ((.) <$> df <*> dg) dx

newtype Updater c a = Updater (c -> a -> Diff a)

newtype Render a = Render (Diff a -> UI Element -> UI Element)

render :: (a -> UI Element) -> Render a
render f = Render $ \d ui -> case d of
    Updated a -> f a
    NoChanges -> ui

update :: u -> a -> Maybe a
update = undefined



data Foo = Foo Int
         | Bar Foo Int
         | Baz Foo Foo

-- foo :: Updater Int Foo
-- foo = Updater $ \x a -> case a of
