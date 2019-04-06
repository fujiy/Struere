{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Data.Struere.Editor.Struere where

import           Control.Monad.State
import qualified Data.Text                        as T

import qualified Data.Struere.Position            as Pos
import           Data.Struere.Util
-- import           Data.Struere.Editor.View


import           Data.Struere.Editor.Carpenter
import           Data.Struere.Editor.Mode.Haskell
import           Data.Struere.Struct
import           Data.Struere.Syntax

-- data Editor = Editor
--     { buffer :: Buffer
--     }

data Input = InputChar Char
           | KeyCode Int
    deriving (Eq, Show)

data Context = Context
    { carets    :: Carets
    , struct    :: Struct
    , value     :: Test
    , updater   :: Updater Test
    , syntax    :: forall f. Syntax f => f Test
    , blueprint :: Blueprint
    }


-- data Buffer = Buffer
--     { name      :: T.Text
--     , carpenter :: Carpenter Test
--     }

newtype Struere a = Struere
    { runStruere :: Context -> (a, Context) }
    deriving (Functor)

-- instance Applicative Struere where
--     pure a = Struere $ \c e -> (a, c, e)

-- modify :: (Editor -> Editor) -> Struere ()
-- modify f = Struere $ \c e -> ((), c, f e)

-- modifyContext :: (Context -> Context) -> Struere ()
-- modifyContext f = Struere $ \c e -> ((), f c, e)

-- replace :: Event -> Struere ()
-- replace = return ()
