{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Struere.Editor.Editable
    ( Editable(..)
    ) where

import qualified Data.Text                      as T


import           Control.Isomorphism.Partial
import           Control.Isomorphism.Partial.TH

import           Data.Struere.Editor.Brick
import           Data.Struere.Parser
import           Data.Struere.Syntax



class Editable a where
    type Pre a :: *
    type Pre a = ()

    structure :: Syntax f => f a

    defaultPre :: Pre a

    preprocessor :: T.Text -> Pre a
    preprocessor _ = defaultPre @a

    parser    :: T.Text -> Either String a
    parser    = runParser structure

    printer   :: a -> T.Text

