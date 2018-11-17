
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Proxy
import Text.Regex
-- import Elm
import Data.Struere.Scaffold
import Data.Struere.Editor.Protocol
import Data.Struere.Editor.Brick

import Data.Aeson.Types (defaultTaggedObject)
import Elm.Derive
import Elm.Module


main :: IO ()
main = do
   let p = makeElmModule "Protocol"
             [ DefineElm (Proxy :: Proxy Message)
             , DefineElm (Proxy :: Proxy Event)
             , DefineElm (Proxy :: Proxy (Diff Brick)) ]
       p' = addImport "Protocol" "import Brick exposing(..)" p
       s = makeElmModule "Brick"
             [ DefineElm (Proxy :: Proxy Brick)
             , DefineElm (Proxy :: Proxy MetaInfo)]

   writeFile "elm/Protocol.elm" $ fixmod "Protocol" p'
   writeFile "elm/Brick.elm" $ fixmod "Brick" s


fixmod :: String -> String -> String
fixmod mod s =
    let s0 = subRegex (mkRegex "Json\\.Encode\\.list") s "encList"
        s1 = s0 ++ "\nencList f a = Json.Encode.list (map f a)\n"
        s2 = addImport mod "import Json.Decode.Pipeline exposing (..)\nimport List exposing (map)" s1
        -- s2 = subRegex (mkRegex "import Set") s1
                       -- "import Json.Decode.Pipeline exposing (..)\nimport List exposing (map)\nimport Set"
           in s2

addImport :: String -> String -> String -> String
addImport mod s to =
    let m  = "module " ++ mod ++ " exposing\\(\\.\\.\\)"
        m' = "module " ++ mod ++ " exposing(..)"
    in  subRegex (mkRegex m) to (m' ++ "\n\n" ++ s ++ "\n")
-- main = specsToDir [spec] "elm"

-- spec :: Spec
-- spec = Spec ["Protocol"]
--     [ toElmTypeSource    $ Proxy @Instr
--     -- , toElmDecoderSource $ Proxy @Instr
--     -- , toElmEncoderSource $ Proxy @Instr
--     , toElmTypeSource    $ Proxy @Event
--     , toElmTypeSource    $ Proxy @Structure
--     ]
