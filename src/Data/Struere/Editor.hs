
module Data.Struere.Editor
    ( main
    ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import qualified Network.WebSockets as WS

import Data.Struere.Editor.Protocol


main :: IO ()
main = do
    return ()
    -- print $ encode (Instr 2 False)
    WS.runServer "0.0.0.0" 9160 application

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    loop conn


loop :: WS.Connection -> IO ()
loop conn = do
    msg <- WS.receiveData conn :: IO BS.ByteString
    print (decode msg :: Maybe Event)
    loop conn
