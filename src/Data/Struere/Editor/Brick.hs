{-# LANGUAGE DeriveGeneric #-}

module Data.Struere.Editor.Brick where

import qualified Data.Text as T


data Brick
    = Hole MetaInfo
    | Empty
    | Plane Char
    -- | Keyword T.Text
    -- | Operator T.Text
    -- | Cons Brick Brick
    | Array [Brick]
    | Meta MetaInfo Brick

data MetaInfo = MetaInfo
    { cursor :: Bool }
    deriving (Show )

consBrick :: Brick -> Brick -> Brick
-- Plane xs `consBrick` Plane ys = Plane $ xs ++ ys
Empty    `consBrick` b        = b
a        `consBrick` Empty    = a
Array xs `consBrick` Array ys = Array $ xs ++ ys
Array xs `consBrick` b        = Array $ xs ++ [b]
a        `consBrick` Array ys = Array $ a:ys
a        `consBrick` b        = Array [a, b]


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
