module Brick exposing(..)

import Json.Decode.Pipeline exposing (..)
import List exposing (map)


import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type Brick  =
    Hole 
    | Keyword String
    | Operator String
    | Array Int (List Brick)
    | Meta MetaInfo Brick

jsonDecBrick : Json.Decode.Decoder ( Brick )
jsonDecBrick =
    let jsonDecDictBrick = Dict.fromList
            [ ("Hole", Json.Decode.lazy (\_ -> Json.Decode.succeed Hole))
            , ("Keyword", Json.Decode.lazy (\_ -> Json.Decode.map Keyword (Json.Decode.string)))
            , ("Operator", Json.Decode.lazy (\_ -> Json.Decode.map Operator (Json.Decode.string)))
            , ("Array", Json.Decode.lazy (\_ -> Json.Decode.map2 Array (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (Json.Decode.list (jsonDecBrick)))))
            , ("Meta", Json.Decode.lazy (\_ -> Json.Decode.map2 Meta (Json.Decode.index 0 (jsonDecMetaInfo)) (Json.Decode.index 1 (jsonDecBrick))))
            ]
        jsonDecObjectSetBrick = Set.fromList []
    in  decodeSumTaggedObject "Brick" "tag" "contents" jsonDecDictBrick jsonDecObjectSetBrick

jsonEncBrick : Brick -> Value
jsonEncBrick  val =
    let keyval v = case v of
                    Hole  -> ("Hole", encodeValue (encList identity []))
                    Keyword v1 -> ("Keyword", encodeValue (Json.Encode.string v1))
                    Operator v1 -> ("Operator", encodeValue (Json.Encode.string v1))
                    Array v1 v2 -> ("Array", encodeValue (encList identity [Json.Encode.int v1, (encList jsonEncBrick) v2]))
                    Meta v1 v2 -> ("Meta", encodeValue (encList identity [jsonEncMetaInfo v1, jsonEncBrick v2]))
    in encodeSumTaggedObject "tag" "contents" keyval val



type alias MetaInfo  =
   { cursor: Bool
   }

jsonDecMetaInfo : Json.Decode.Decoder ( MetaInfo )
jsonDecMetaInfo =
   Json.Decode.succeed (\pcursor -> {cursor = pcursor}) |> custom (Json.Decode.bool)

jsonEncMetaInfo : MetaInfo -> Value
jsonEncMetaInfo  val =
   Json.Encode.bool val.cursor

encList f a = Json.Encode.list (map f a)
