{-# LANGUAGE TemplateHaskell #-}

module Data.Struere.Editor.Brick where

import qualified Data.Text as T
import           Data.Aeson       (FromJSON, ToJSON)
import Data.Aeson.Types (defaultTaggedObject)
import Elm.Derive
import Elm.Module


data Brick
    = Hole
    | Keyword T.Text
    | Operator T.Text
    | Array Int [Brick]
    | Meta MetaInfo Brick
    deriving (Show)

data MetaInfo = MetaInfo
    { cursor :: Bool }
    deriving (Show)

deriveBoth (defaultOptions { sumEncoding = defaultTaggedObject })
    ''MetaInfo


deriveBoth (defaultOptions { sumEncoding = defaultTaggedObject })
    ''Brick

-- instance ToJSON   Brick
-- instance FromJSON Brick

    -- | Cons  { level    :: Int
    --         , symbols  :: [T.Text]
    --         , children :: [Brick]
    --         }
    -- | Repeat { level     :: Int
    --          , separator :: T.Text
    --          , children  :: [Brick]
    --          }
    -- | Repeat Int T.Text   [Brick]
