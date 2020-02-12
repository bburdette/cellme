module MdMain exposing (main)

import Browser
import Cellme exposing (Cell, CellContainer(..), CellState, RunState(..), evalCellsFully, evalCellsOnce)
import Dict exposing (Dict)
import DictCellme exposing (CellDict(..), DictCell, dictCc, dictCcr, getCd, mkCc)
import Element exposing (Element)
import Element.Background as EBk
import Element.Border as EBd
import Element.Font as Font
import Element.Input as EI
import Element.Region
import Html exposing (Attribute, Html)
import Html.Attributes
import Markdown.Block exposing (Block(..), Inline, InlineStyle)
import Markdown.Html
import Markdown.Parser
import Show exposing (showTerm, showTerms)


type Msg
    = OnMarkdownInput String
    | OnSchelmeCodeChanged String String


type alias Flags =
    ()


type alias Model =
    { md : String
    , cells : CellDict
    }


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title = "schelme markdown demo"
    , body =
        [ Element.layout [ Element.width Element.fill ]
            (Element.row [ Element.width Element.fill ]
                [ EI.multiline [ Element.width (Element.px 400) ]
                    { onChange = OnMarkdownInput
                    , text = model.md
                    , placeholder = Nothing
                    , label = EI.labelHidden "Markdown input"
                    , spellcheck = False
                    }
                , case markdownView (mkRenderer model.cells) model.md of
                    Ok rendered ->
                        Element.column
                            [ Element.spacing 30
                            , Element.padding 80
                            , Element.width (Element.fill |> Element.maximum 1000)
                            , Element.centerX
                            ]
                            rendered

                    Err errors ->
                        Element.text errors
                ]
            )
        ]
    }


markdownView : Markdown.Parser.Renderer (Element Msg) -> String -> Result String (List (Element Msg))
markdownView renderer markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.andThen (Markdown.Parser.render renderer)


mdCells : String -> Result String CellDict
mdCells markdown =
    markdown
        |> Markdown.Parser.parse
        |> Result.mapError (\error -> error |> List.map Markdown.Parser.deadEndToString |> String.join "\n")
        |> Result.map blockCells


blockCells : List Block -> CellDict
blockCells blocks =
    blocks
        |> List.filterMap
            (\block ->
                case block of
                    Html tag attribs _ ->
                        if tag == "cell" then
                            let
                                am =
                                    Dict.fromList <| List.map (\trib -> ( trib.name, trib.value )) attribs
                            in
                            am
                                |> Dict.get "name"
                                |> Maybe.andThen
                                    (\name ->
                                        am
                                            |> Dict.get "schelmeCode"
                                            |> Maybe.map (String.replace "/'" "\"")
                                            |> Maybe.andThen
                                                (\schelme ->
                                                    Just ( name, defCell schelme )
                                                )
                                    )

                        else
                            Nothing

                    _ ->
                        Nothing
            )
        |> Dict.fromList
        |> CellDict


defCell : String -> DictCell
defCell s =
    { code = s, prog = Err "", runstate = RsErr "" }


mkRenderer : CellDict -> Markdown.Parser.Renderer (Element Msg)
mkRenderer cellDict =
    { heading = heading
    , raw =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , plain = Element.text
    , bold = \content -> Element.row [ Font.bold ] [ Element.text content ]
    , italic = \content -> Element.row [ Font.italic ] [ Element.text content ]
    , code = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink
                [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                        body
                }
                |> Ok
    , image =
        \image body ->
            Element.image [ Element.width Element.fill ] { src = image.src, description = body }
                |> Ok
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop ]
                                    (Element.text "• " :: itemBlocks)
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt index ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html =
        Markdown.Html.oneOf
            [ Markdown.Html.tag "cell"
                (\name schelmeCode renderedChildren ->
                    cellView cellDict renderedChildren name schelmeCode
                )
                |> Markdown.Html.withAttribute "name"
                |> Markdown.Html.withAttribute "schelmeCode"
            ]
    }


cellView : CellDict -> List (Element Msg) -> String -> String -> Element Msg
cellView (CellDict cellDict) renderedChildren name schelmeCode =
    Element.column
        [ EBd.shadow
            { offset = ( 0.3, 0.3 )
            , size = 2
            , blur = 0.5
            , color = Element.rgba255 0 0 0 0.22
            }
        , Element.padding 20
        , Element.spacing 30
        , Element.centerX
        , Font.center
        ]
        (Element.row [ Element.spacing 20 ]
            [ Element.el
                [ Font.bold
                , Font.size 30
                ]
                (Element.text name)
            , EI.text []
                { onChange = OnSchelmeCodeChanged name
                , placeholder = Nothing
                , label = EI.labelHidden name
                , text =
                    cellDict
                        |> Dict.get name
                        |> Maybe.map .code
                        |> Maybe.withDefault "<err>"
                }
            , cellDict
                |> Dict.get name
                |> Maybe.map showRunState
                |> Maybe.withDefault
                    (Element.text "<reserr>")
            ]
            :: renderedChildren
        )


showRunState : DictCell -> Element Msg
showRunState cell =
    Element.el [ Element.width Element.fill ] <|
        case cell.runstate of
            RsOk term ->
                Element.text <| showTerm term

            RsErr s ->
                Element.el [ Font.color <| Element.rgb 1 0.1 0.1 ] <| Element.text <| "err: " ++ s

            RsUnevaled ->
                Element.text <| "unevaled"

            RsBlocked _ id ->
                Element.text <| "blocked on cell: " ++ dictCcr.showId id


rawTextToId : String -> String
rawTextToId rawText =
    rawText
        |> String.toLower
        |> String.replace " " ""


heading : { level : Int, rawText : String, children : List (Element msg) } -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ Font.size
            (case level of
                1 ->
                    36

                2 ->
                    24

                _ ->
                    20
            )
        , Font.bold
        , Font.family [ Font.typeface "Montserrat" ]
        , Element.Region.heading level
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Font.center
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


code : String -> Element msg
code snippet =
    Element.el
        [ EBk.color
            (Element.rgba 0 0 0 0.04)
        , EBd.rounded 2
        , Element.paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ EBk.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.padding 20
        , Element.width Element.fill
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text details.body)


markdownBody : String
markdownBody =
    """# Markdown Schelme Cells!

<cell
  name="inches"
  schelmeCode="5"
>
</cell>

<cell
  name="millimeters"
  schelmeCode="(* (cv /'inches/') 25.4)"
>
</cell>

<cell
  name="furlongs"
  schelmeCode="(/ (cv /'inches/') (* 12 660))"
>
</cell>
"""


main : Platform.Program Flags Model Msg
main =
    Browser.document
        { init =
            \flags ->
                let
                    cells =
                        Debug.log "newcells"
                            (markdownBody
                                |> mdCells
                                |> Result.withDefault (CellDict Dict.empty)
                            )

                    ( cc, result ) =
                        evalCellsFully
                            (mkCc cells)
                in
                ( { md = markdownBody
                  , cells = Debug.log "evaled cells: " <| getCd cc
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMarkdownInput newMarkdown ->
            let
                cells =
                    Debug.log "newcells"
                        (newMarkdown
                            |> mdCells
                            |> Result.withDefault (CellDict Dict.empty)
                        )

                ( cc, result ) =
                    evalCellsFully
                        (mkCc cells)
            in
            ( { model
                | md = newMarkdown
                , cells = Debug.log "evaled cells: " <| getCd cc
              }
            , Cmd.none
            )

        OnSchelmeCodeChanged name string ->
            let
                (CellDict cd) =
                    model.cells

                ( cc, result ) =
                    evalCellsFully
                        (mkCc
                            (Dict.insert name (defCell string) cd
                                |> CellDict
                            )
                        )
            in
            ( { model
                | cells = getCd cc
              }
            , Cmd.none
            )
