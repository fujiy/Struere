
{-# LANGUAGE ExistentialQuantification #-}

module Data.Struere.Editor.Mode.Major where

import qualified Data.Text as T

import           Data.Struere.Editor.Editable


data MajorMode a = MajorMode a
-- data MajorMode = forall m. IsMajorMode m => MajorMode m


-- class IsMajorMode m where
--     name :: m -> T.Text
