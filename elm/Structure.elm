module Structure exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import List exposing (map)
import Set exposing (Set)


type Structure  =
    Hole 
    | Cons (List Structure)
    | Repeat (List Structure)

jsonDecStructure : Json.Decode.Decoder ( Structure )
jsonDecStructure =
    let jsonDecDictStructure = Dict.fromList
            [ ("Hole", Json.Decode.lazy (\_ -> Json.Decode.succeed Hole))
            , ("Cons", Json.Decode.lazy (\_ -> Json.Decode.map Cons (Json.Decode.list (jsonDecStructure))))
            , ("Repeat", Json.Decode.lazy (\_ -> Json.Decode.map Repeat (Json.Decode.list (jsonDecStructure))))
            ]
        jsonDecObjectSetStructure = Set.fromList []
    in  decodeSumTaggedObject "Structure" "tag" "contents" jsonDecDictStructure jsonDecObjectSetStructure

jsonEncStructure : Structure -> Value
jsonEncStructure  val =
    let keyval v = case v of
                    Hole  -> ("Hole", encodeValue (encList identity []))
                    Cons v1 -> ("Cons", encodeValue ((encList jsonEncStructure) v1))
                    Repeat v1 -> ("Repeat", encodeValue ((encList jsonEncStructure) v1))
    in encodeSumTaggedObject "tag" "contents" keyval val


encList f a = Json.Encode.list (map f a)
