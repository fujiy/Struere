{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Struere.Scaffold where


import Control.Arrow
import Control.Monad.State
import GHC.Generics
import Data.Aeson.Types (defaultTaggedObject)
import Elm.Derive
import Elm.Module


-- Transparent

class Monad io => Shared io a where
    sendBy :: Scaffold s => s -> (s -> a) -> io ()

class (Monad io, Scaffold s) => Remote io s where
    recieve :: io s

updateT :: (Scaffold s, Shared io a)
        => (s -> Maybe s) -> (s -> a) -> ProcessT s io ()
updateT f g =
    updateM $ f >>> \case
        Nothing -> return Nothing
        Just s' -> sendBy s' g >> return (Just s')


-- Scaffold

class Scaffold s where
    work :: Monad m => (s -> m s) -> s -> m s

-- Process

type ProcessT s m a = StateT ([History s], s) m a

runProcess :: Monad m => s -> ProcessT s m a -> m a
runProcess s m = evalStateT m ([], s)

data Process s = Process
    { now     :: s
    , history :: [History s]
    } deriving (Show)

initP :: s -> Process s
initP s = Process s []

updateM :: (Monad m, Scaffold s)
        => (s -> m (Maybe s)) -> ProcessT s m ()
updateM f = do
    (h, s) <- get
    -- (ds, s') <- runUpdater
    --     (work (\a -> changed a send $ lift (f a)
    --         lift (f a) >>= \s' -> send <$> s'
    --         ) s)
    --     zeroPos
    -- return ()
    -- m <- lift $ f s
    -- case m of
    --     Nothing -> return ()
    --     Just s' -> do
    --         (ds, s') <- runUpdater
    (ds, s') <- lift $
          runUpdater
          (work (\a -> changed a $ f a) s)
          zeroPos
    put (ds : h, s')
    -- return s'

-- update :: forall m s . (Monad m, Scaffold s)
--        => (s -> m (Maybe s)) -> Process s -> m (Process s)
-- update f p = do
--     (ds, s') <-
--           runUpdater
--           (work (\s -> changed s $ f s) $ now p)
--           zeroPos
--     return $ p { now = s'
--                , history = ds : history p }


changed :: Monad m => s -> m (Maybe s) -> Updater s m s
changed before mm =
    Updater $ \p -> do
        m <- mm
        return $ case m of
            Nothing -> ([], before)
            Just s' -> ([Diff p before], s')
     -- (return . maybe
     --     ([], before)
     --     ([Diff p before] ,) ) m
     -- return ([Diff p before], a)

-- Updator

newtype Updater d m s = Updater
    { runUpdater :: Position -> m ([Diff d], s) }

instance Monad m => Functor (Updater d m) where
    f `fmap` m = Updater $ \p -> do
        (d, a) <- runUpdater m p
        return (d, f a)

instance Monad m => Applicative (Updater d m) where
    pure s = Updater $ const $ return ([], s)
    fm <*> m = Updater $ \p -> do
        (df, f) <- runUpdater fm p
        (da, a) <- runUpdater m (incPos p)
        return (df <> da, f a)

instance Monad m => Monad (Updater d m) where
    m >>= f = Updater $ \p -> do
        (d,  a)  <- runUpdater m (subPos p)
        (d', a') <- runUpdater (f a) p
        return (d <> d', a')


-- History

type History s = [Diff s]

data Diff s = Diff
    { pos   :: [Int]
    , value :: s
    } deriving (Show, Generic)

type Position = [Int]


-- instance ToJSON   a => ToJSON   (Diff a)
-- instance FromJSON a => FromJSON (Diff a)

-- Position


incPos :: Position -> Position
incPos (x:xs) = x + 2 : xs

subPos :: Position -> Position
subPos = (0 :)

zeroPos :: Position
zeroPos = [0]

deriveBoth (defaultOptions { sumEncoding = defaultTaggedObject })
    ''Diff


data Sample
    = Foo Int
    | Bar Sample
    deriving (Generic, Show)

instance Scaffold Sample where
    work f (Bar s) = work f s >>= f . Bar
    work f a = f a
    -- work f (Bar s) = Bar <$> work f s
foo :: Sample -> IO (Maybe Sample)
foo (Foo i) = do
    print i
    return $ Just $ Foo $ i + 1
foo s = return Nothing
