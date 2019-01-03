
module Data.Struere.Editor.Struere where

import           Control.Monad.State
import qualified Data.Text                        as T

import           Data.Struere.Editor.Carpenter
import qualified Data.Struere.Editor.Position     as Pos
import           Data.Struere.Editor.Util
-- import           Data.Struere.Editor.View


import           Data.Struere.Editor.Mode.Haskell

-- data Editor = Editor
--     { buffer :: Buffer
--     }

data Context = Context
    { carets    :: Carets
    , carpenter :: Carpenter Test
    }

type Carets = Pos.Positioned ()

-- data Buffer = Buffer
--     { name      :: T.Text
--     , carpenter :: Carpenter Test
--     }

-- newtype Struere a = Struere
--     { runStruere :: Context -> Editor -> (a, Context, Editor) }
--     deriving (Functor)

-- instance Applicative Struere where
--     pure a = Struere $ \c e -> (a, c, e)

-- modify :: (Editor -> Editor) -> Struere ()
-- modify f = Struere $ \c e -> ((), c, f e)

-- modifyContext :: (Context -> Context) -> Struere ()
-- modifyContext f = Struere $ \c e -> ((), f c, e)

-- replace :: Event -> Struere ()
-- replace = return ()
