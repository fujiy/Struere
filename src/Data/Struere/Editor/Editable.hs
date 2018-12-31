
module Data.Struere.Editor.Editable
    ( Editable(..)
    ) where

import qualified Data.Text as T


import           Control.Isomorphism.Partial
import           Control.Isomorphism.Partial.TH

import           Data.Struere.Structural
import           Data.Struere.Parser
import           Data.Struere.Editor.Brick



class Editable a where
    structure :: Structural f => f a

    parser    :: T.Text -> Either String a
    parser    = runParser structure

    printer   :: a -> T.Text

