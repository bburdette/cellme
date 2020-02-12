module DictMain exposing (Model, Msg(..), eview, initelts, main, update, view, viewCell)

import Browser
import Browser.Dom as BD exposing (Element, getElement)
import Browser.Events as BE
import Browser.Navigation as BN
import Cellme.Cellme exposing (Cell, CellContainer(..), CellState, RunState(..), evalCellsFully, evalCellsOnce)
import Cellme.DictCellme exposing (CellDict(..), DictCell, dictCc, getCd, mkCc)
import Dict exposing (Dict)
import Element as E exposing (Element, centerX, column, el, fill, fillPortion, height, image, inFront, indexedTable, map, newTabLink, padding, paddingXY, paragraph, rgb, rgba, row, shrink, spacing, table, text, width)
import Element.Background as BD
import Element.Border as Border
import Element.Events as EE
import Element.Font as Font
import Element.Input as EI
import EvalStep exposing (NameSpace, Term(..))
import Prelude exposing (BuiltInFn)
import Run exposing (compile, runCount)
import Show exposing (showTerm, showTerms)
import Toop as T


type Msg
    = Noop
    | CellVal String String
    | EvalButton
    | RunButton


type alias Model =
    { elts : CellDict }


initelts : CellDict
initelts =
    let
        cd =
            [ ( "millimeters", "15" )
            , ( "inches", "(/ (cv \"millimeters\") 25.4)" )
            ]
                |> List.map
                    (\( k, s ) ->
                        ( k
                        , { code = s
                          , prog = Err ""
                          , runstate = RsUnevaled
                          }
                        )
                    )
                |> Dict.fromList
    in
    CellDict cd


eview : Model -> Element Msg
eview model =
    let
        (CellDict mca) =
            model.elts
    in
    column [ width fill, height fill, spacing 5, padding 5 ]
        [ newTabLink []
            { url = "https://github.com/bburdette/elm-sheet/"
            , label = el [ Font.color (rgb 0 0 0.6) ] <| text "elm-sheet on github"
            }
        , row [ spacing 5 ]
            [ EI.button
                [ BD.color (rgb 0.5 0.5 0.5)
                , Font.color (rgb 1 1 1)
                , Border.color (rgb 0 0 0.6)
                , paddingXY 5 3
                , Border.rounded 5
                ]
                { onPress = Just EvalButton
                , label = text "step"
                }
            , EI.button
                [ BD.color (rgb 0.5 0.5 0.5)
                , Font.color (rgb 1 1 1)
                , Border.color (rgb 0 0 0.6)
                , paddingXY 5 3
                , Border.rounded 5
                ]
                { onPress = Just RunButton
                , label = text "run"
                }
            ]
        , table []
            { data = Dict.toList mca
            , columns =
                [ { header = text "Name"
                  , width = fill
                  , view =
                        \( k, v ) ->
                            text k
                  }
                , { header = text "Code"
                  , width = fill
                  , view =
                        \( k, v ) ->
                            viewCell k v
                  }
                ]
            }
        ]


viewCell : String -> DictCell -> Element Msg
viewCell key cell =
    let
        (CellContainer mycc) =
            dictCc
    in
    column [ width fill ]
        [ EI.text [ width fill ]
            { onChange = \v -> CellVal key v
            , text = cell.code
            , placeholder = Nothing
            , label = EI.labelHidden ("cell '" ++ key ++ "'")
            }
        , el [ width fill ] <|
            case cell.runstate of
                RsOk term ->
                    text <| showTerm term

                RsErr s ->
                    el [ Font.color <| rgb 1 0.1 0.1 ] <| text <| "err: " ++ s

                RsUnevaled ->
                    text <| "unevaled"

                RsBlocked _ id ->
                    text <| "blocked on cell: " ++ mycc.showId id
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "schelmesheet"
    , body =
        [ E.layout [] <| eview model
        ]
    }


defCell : String -> DictCell
defCell s =
    { code = s, prog = Err "", runstate = RsErr "" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        CellVal key val ->
            let
                (CellDict mca) =
                    model.elts
            in
            ( { model
                | elts =
                    Dict.insert key (defCell val) mca
                        |> CellDict
              }
            , Cmd.none
            )

        EvalButton ->
            ( { model | elts = getCd <| evalCellsOnce (mkCc model.elts) }, Cmd.none )

        RunButton ->
            let
                ( cells, result ) =
                    evalCellsFully (mkCc model.elts)
            in
            ( { model | elts = getCd cells }, Cmd.none )


main : Platform.Program () Model Msg
main =
    Browser.document
        { init = \() -> ( { elts = initelts }, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        , update = update
        }
