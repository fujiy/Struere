module Main exposing (main)

import Browser
import Html exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type alias Msg =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( (), Cmd.none )


view : Model -> Html Msg
view model =
    p [] [ text "Hello" ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
