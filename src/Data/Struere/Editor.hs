{-# LANGUAGE OverloadedStrings #-}
module Data.Struere.Editor
    ( main
    ) where

import           Data.Aeson                       hiding (Result)
import qualified Data.ByteString.Lazy             as BS
import           Data.Char
import           Data.Dynamic
import qualified Data.Text                        as T
import qualified Network.WebSockets               as WS
import           Prelude                          hiding (pure, (<$>), (<*>))

import           Data.Struere.Editor.Brick
import           Data.Struere.Editor.Protocol
import           Data.Struere.Editor.View

import           Data.Struere.Editor.Carpenter
import           Data.Struere.Isomorphism
import           Data.Struere.Structural
-- import Data.Struere.Viewer
import           Data.Struere.Editor.Editable
import           Data.Struere.Editor.Mode.Haskell

main :: IO ()
main = do
    return ()
    -- print $ encode (Instr 2 False)
    -- let Right hask = parser "a = a" :: Either String HaskellFile
    --     r          = runCarpenter structure
    --                  [Root $ ISet $ toDyn hask] :: Result HaskellFile
        -- initc = Context scaf

    -- let
    --     hoge = name1
    --     str = name
    --     -- str = sub defaultLevel
    --     --     $ (cons <$> letter <*> pure [])
    --     --     <|> (nil <$> pure ())
    --     Just (b, c0) = builder str hoge
    --     (a1, d1, c1) = updater c0 [] -- [Root $ ISet $ toDyn ("bcd" :: String)]
    --     (a2, d2, c2) = updater c1
    --                    -- [Root $ ISet $ toDyn '1']
    --                    -- [Root $ ISet $ toDyn ("xy" :: String)]
    --                    -- []
    --                    [ Sub 0 [Sub 0 [Root $ ISet $ toDyn 'a']]
    --                    , Sub 0 [Sub 2 [Root $ ISet $ toDyn 'v']]]
    --                    -- [Sub 0 []]
    --     (a3, d3, c3) = updater c2 [Root $ ISet $ toDyn ("foo" :: String)]

    -- print hoge
    -- print b
    -- putStrLn ""
    -- print a1
    -- print d1
    -- putStrLn ""
    -- print a2
    -- print d2
    -- putStrLn ""
    -- print a3
    -- print d3
    -- print "end"

    WS.runServer "0.0.0.0" 9160 application

application :: WS.ServerApp
application pending = do

    -- let Right hask = parser "a = a"
    --     Just scaf  = runBuilder structure hask
    --     initc = Context scaf

    -- print hask

    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    -- let c = Context $ carpenter

    let Just (b, c) = builder test test1

    let msg = LoadBuffer 0 $ Buffer "test" b
    WS.sendTextData conn $ encode msg

    loop conn $ Context { carpenter = c }


loop :: WS.Connection -> Context -> IO ()
loop conn c = do
    msg <- WS.receiveData conn :: IO BS.ByteString

    let mevent = decode msg :: Maybe Event
        is = case mevent of
            Just (KeyPress km x) ->
                let p = fromEnum x
                in  [Sub 0 [Root $ ISet $ toDyn p]]
            _                    -> []
        (ma, d, c') = updater (carpenter c) is

    -- let r = runCarpenter (carpenter c) Nothing []

    print (decode msg :: Maybe Event)
    loop conn c

data Context = Context
    { carpenter :: Carpenter Test
    }

data Struere
