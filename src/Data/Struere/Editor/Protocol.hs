{-# LANGUAGE DeriveGeneric #-}

module Data.Struere.Editor.Protocol where

import           Data.Aeson.Types          (FromJSON, ToJSON)
import           GHC.Generics

-- import Data.Struere.Scaffold
import           Data.Struere.Editor.Brick
import           Data.Struere.Editor.View

-- data Instr
--     = Instr Int Bool
--     | Other Int

data Message
    = LoadBuffer Int Buffer
    | UpdateBuffer [Int] Brick
    deriving (Generic)


instance ToJSON   Message
instance FromJSON Message



-- instance ToJSON   Message
-- instance FromJSON Message
-- instance ElmType  Instr

data Event
    = KeyPress KeyModifier Int
    deriving (Show, Generic)

instance ToJSON   Event
instance FromJSON Event


data KeyModifier = KeyModifier
    { altKey   :: Bool
    , ctrlKey  :: Bool
    , shiftKey :: Bool
    , metaKey  :: Bool
    } deriving (Eq, Show, Generic)

instance ToJSON   KeyModifier
instance FromJSON KeyModifier


type Position = [Int]

subPos :: Int -> Position -> Position
subPos = (:)

zeroPos :: Position
zeroPos = [0]

rootPos :: Position
rootPos = []
