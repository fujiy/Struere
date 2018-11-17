module Protocol exposing(..)

import Json.Decode.Pipeline exposing (..)
import List exposing (map)


import Brick exposing(..)


import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type Message  =
    LoadBrick Brick
    | UpdateBrick (Diff Brick)

jsonDecMessage : Json.Decode.Decoder ( Message )
jsonDecMessage =
    let jsonDecDictMessage = Dict.fromList
            [ ("LoadBrick", Json.Decode.lazy (\_ -> Json.Decode.map LoadBrick (jsonDecBrick)))
            , ("UpdateBrick", Json.Decode.lazy (\_ -> Json.Decode.map UpdateBrick (jsonDecDiff (jsonDecBrick))))
            ]
        jsonDecObjectSetMessage = Set.fromList []
    in  decodeSumTaggedObject "Message" "tag" "contents" jsonDecDictMessage jsonDecObjectSetMessage

jsonEncMessage : Message -> Value
jsonEncMessage  val =
    let keyval v = case v of
                    LoadBrick v1 -> ("LoadBrick", encodeValue (jsonEncBrick v1))
                    UpdateBrick v1 -> ("UpdateBrick", encodeValue ((jsonEncDiff (jsonEncBrick)) v1))
    in encodeSumTaggedObject "tag" "contents" keyval val



type Event  =
    KeyPress Int

jsonDecEvent : Json.Decode.Decoder ( Event )
jsonDecEvent =
    Json.Decode.lazy (\_ -> Json.Decode.map KeyPress (Json.Decode.int))


jsonEncEvent : Event -> Value
jsonEncEvent (KeyPress v1) =
    Json.Encode.int v1



type alias Diff s =
   { pos: (List Int)
   , value: s
   }

jsonDecDiff : Json.Decode.Decoder s -> Json.Decode.Decoder ( Diff s )
jsonDecDiff localDecoder_s =
   Json.Decode.succeed (\ppos pvalue -> {pos = ppos, value = pvalue})
   |> required "pos" (Json.Decode.list (Json.Decode.int))
   |> required "value" (localDecoder_s)

jsonEncDiff : (s -> Value) -> Diff s -> Value
jsonEncDiff localEncoder_s val =
   Json.Encode.object
   [ ("pos", (encList Json.Encode.int) val.pos)
   , ("value", localEncoder_s val.value)
   ]


encList f a = Json.Encode.list (map f a)
