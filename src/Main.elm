module Main exposing (Model, Msg(..), eview, initelts, main, update, view, viewCell)

import Array exposing (Array)
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
    | CellVal Int Int String


type alias Model =
    { elts : Array (Array String) }


initelts =
    Array.fromList
        [ Array.fromList [ "1", "7", "8" ]
        , Array.fromList [ "2", "5", "6" ]
        , Array.fromList [ "9", "0", "0" ]
        ]


eview : Model -> Element Msg
eview model =
    let
        colf =
            \colidx ->
                { header = text (String.fromInt colidx)
                , width = fill
                , view =
                    \rowidx array ->
                        Array.get colidx array
                            |> Maybe.map
                                (viewCell colidx rowidx)
                            |> Maybe.withDefault (text "err")
                }
    in
    indexedTable [ width fill, height fill ]
        { data = Array.toList model.elts
        , columns =
            List.map colf (List.range 0 (Array.length model.elts - 1))
        }


viewCell : Int -> Int -> String -> Element Msg
viewCell xi yi val =
    EI.text [ width fill ]
        { onChange = \v -> CellVal xi yi v
        , text = val
        , placeholder = Nothing
        , label = EI.labelHidden ("cell" ++ String.fromInt xi ++ "," ++ String.fromInt yi)
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
    case msg of
        Noop ->
            ( model, Cmd.none )

        CellVal xi yi val ->
            let
                _ =
                    Debug.log "CellVal" ( xi, yi, val )
            in
            ( { model
                | elts =
                    Array.get yi model.elts
                        |> Maybe.map
                            (\rowarray ->
                                Debug.log "elts:" <|
                                    Array.set yi (Array.set xi val rowarray) model.elts
                            )
                        |> Maybe.withDefault model.elts
              }
            , Cmd.none
            )


main =
    Browser.document
        { init = \() -> ( { elts = initelts }, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
