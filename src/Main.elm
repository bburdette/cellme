module Main exposing (Model, elts, eview, view)

import Browser
import Browser.Dom as BD exposing (Element, getElement)
import Browser.Events as BE
import Browser.Navigation as BN
import Dict exposing (Dict)
import Element as E exposing (Element, centerX, column, el, fill, fillPortion, height, image, inFront, indexedTable, map, newTabLink, paragraph, rgb, rgba, row, shrink, spacing, table, text, width)
import Element.Background as BD
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Element.Input as EI
import Toop as T


type Msg
    = Noop


type alias Model =
    { elts : List (T.T3 String String String) }


elts =
    [ T.T3 "1" "3" "4"
    , T.T3 "2" "5" "6"
    , T.T3 "9" "0" "0"
    ]


eview : Model -> Element Msg
eview model =
    indexedTable [ width fill, height fill ]
        { data = elts
        , columns =
            [ { header = text "1"
              , width = fill
              , view = \i (T.T3 a b c) -> row [ BD.color <| rgb 1 0 0 ] [ el [ centerX ] <| text a ]
              }
            , { header = text "2"
              , width = fillPortion 2
              , view = \i (T.T3 a b c) -> row [ BD.color <| rgb 0 1 0 ] [ el [ centerX ] <| text b ]
              }
            , { header = text "3"
              , width = fill
              , view = \i (T.T3 a b c) -> row [ BD.color <| rgb 0 0 1 ] [ el [ centerX ] <| text c ]
              }
            ]
        }


view : Model -> Browser.Document Msg
view model =
    { title = "schelmesheet"
    , body =
        [ E.layout [] <| eview model
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


main =
    Browser.document
        { init = \() -> ( { elts = elts }, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
