{-# LANGUAGE DeriveGeneric #-}

module Data.Struere.Editor.Brick where

import qualified Data.Text as T


data Brick
    = Hole MetaInfo
    | Empty
    | Token Char
    -- | Keyword T.Text
    -- | Operator T.Text
    -- | Cons Brick Brick
    | Array [Brick]
    | Meta MetaInfo Brick
    deriving (Show)

data MetaInfo = MetaInfo
    { cursor :: Bool }
    deriving (Show )

consBrick :: Brick -> Brick -> Brick
-- Plane xs `consBrick` Plane ys = Plane $ xs ++ ys
Empty    `consBrick` b        = b
a        `consBrick` Empty    = a
a        `consBrick` b        = Array [a, b]

mergeArray :: Brick -> Brick -> Brick
Empty    `mergeArray` b        = b
a        `mergeArray` Empty    = a
Array xs `mergeArray` Array ys = Array $ xs ++ ys
Array xs `mergeArray` b        = Array $ xs ++ [b]
a        `mergeArray` Array ys = Array $ a:ys
a        `mergeArray` b        = Array [a, b]


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
