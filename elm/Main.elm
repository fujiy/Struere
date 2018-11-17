module Main exposing (main)

-- import Cmd.Extra exposing (withCmd, withNoCmd)
-- import Html exposing (..)
-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)

import Brick exposing (..)
import Color as Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Html exposing (Html, program)
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard
import List as L
import Protocol exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import WebSocket as WS


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { window : Window }


type Msg
    = Event Event
    | Receive Message
    | None


init : ( Model, Cmd Msg )
init =
    ( Model (Window (Buffer "foo" sample)), Cmd.none )


sample : Brick
sample =
    Hole


server : String
server =
    "ws://0.0.0.0:9160"


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Event e ->
            ( model, WS.send server (Encode.encode 0 <| jsonEncEvent e) )

        _ ->
            ( model, Cmd.none )


type alias Window =
    { buffer : Buffer }


type alias Buffer =
    { name : String
    , main : Brick
    }


window w =
    column WindowStyle
        [ height fill ]
        [ buffer w.buffer
        , statusline w.buffer
        , commandline w.buffer
        ]


statusline b =
    row StatusLineStyle
        [ height (px 20) ]
        [ el TextStyle [] (text b.name) ]


commandline b =
    row CommandLineStyle
        [ height (px 20) ]
        []


buffer b =
    el BufferStyle
        [ height fill ]
        (brick b.main)


brick b =
    case b of
        Hole ->
            el (BS HoleStyle)
                [ height (px 20)
                , width (px 40)
                ]
                empty

        -- Repeat r ->
        --     row (BS RepeatStyle)
        --         []
        --         (L.intersperse (symbol r.separator) <|
        --             L.map brick r.children
        --         )
        _ ->
            text "others"


symbol s =
    el (BS SymbolStyle) [] (text s)


type Style
    = TabsStyle
    | WindowStyle
    | BufferStyle
    | StatusLineStyle
    | CommandLineStyle
    | TextStyle
    | BS BrickStyle


type BrickStyle
    = HoleStyle
    | RepeatStyle
    | SymbolStyle


stylesheet =
    styleSheet
        [ style TabsStyle
            []
        , style WindowStyle
            [ Color.background Color.black ]
        , style StatusLineStyle
            [ Border.top 1
            , Border.bottom 1
            , Color.border Color.darkGray
            ]
        , style CommandLineStyle
            []
        , style TextStyle
            [ Color.text Color.white
            , Font.typeface [ Font.monospace ]
            ]
        , style (BS HoleStyle)
            [ Color.background Color.darkGray ]
        , style (BS SymbolStyle)
            [ Color.text Color.red
            , Font.typeface [ Font.monospace ]
            ]
        ]


view : Model -> Html Msg
view model =
    viewport stylesheet <|
        grid TabsStyle
            [ height fill ]
            { columns = [ fill ]
            , rows = [ fill ]
            , cells =
                [ cell
                    { start = ( 0, 0 )
                    , width = 1
                    , height = 1
                    , content = window model.window
                    }
                ]
            }



-- window : Model -> Html Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ subWS, subKey ]


subWS : Sub Msg
subWS =
    WS.listen server
        (\s ->
            case Decode.decodeString jsonDecMessage s of
                Ok m ->
                    Receive m

                Err s ->
                    None
        )


subKey : Sub Msg
subKey =
    Keyboard.downs (Event << KeyPress)
