{-# LANGUAGE TemplateHaskell #-}

module Data.Struere.Editor.Protocol where

import Data.Aeson.Types (defaultTaggedObject)
import Elm.Derive
import Elm.Module

import Data.Struere.Scaffold
import Data.Struere.Editor.Brick

-- data Instr
--     = Instr Int Bool
--     | Other Int

data Message
    = LoadBrick Brick
    | UpdateBrick (Diff Brick)
    deriving (Show)

deriveBoth (defaultOptions { sumEncoding = defaultTaggedObject })
    ''Message



-- instance ToJSON   Message
-- instance FromJSON Message
-- instance ElmType  Instr

data Event
    = KeyPress Int
    deriving (Show)

deriveBoth (defaultOptions { sumEncoding = defaultTaggedObject })
    ''Event
-- instance ToJSON   Event
-- instance FromJSON Event
-- instance ElmType  Event
