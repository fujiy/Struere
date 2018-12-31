{-# LANGUAGE DeriveGeneric #-}

module Data.Struere.Editor.Brick where

import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Text    as T
import           GHC.Generics


data Brick
    = Hole MetaInfo
    | Empty
    | Plane String
    -- | Keyword T.Text
    -- | Operator T.Text
    -- | Cons Brick Brick
    | Array [Brick]
    | Meta MetaInfo Brick
    deriving (Show, Generic)

instance ToJSON   Brick
instance FromJSON Brick

data MetaInfo = MetaInfo
    { cursor :: Bool }
    deriving (Show, Generic)

consBrick :: Brick -> Brick -> Brick
Plane xs `consBrick` Plane ys = Plane $ xs ++ ys
Empty    `consBrick` b        = b
a        `consBrick` Empty    = a

instance ToJSON   MetaInfo
instance FromJSON MetaInfo

emptyMeta :: MetaInfo
emptyMeta = MetaInfo False

    -- | Cons  { level    :: Int
    --         , symbols  :: [T.Text]
    --         , children :: [Brick]
    --         }
    -- | Repeat { level     :: Int
    --          , separator :: T.Text
    --          , children  :: [Brick]
    --          }
    -- | Repeat Int T.Text   [Brick]
