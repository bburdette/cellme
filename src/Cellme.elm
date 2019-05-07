module Cellme exposing (Cell, CellState(..), CellStatus(..), PRes(..), PSideEffectorFn, RunState(..), cellVal, cellme, evalArgsPSideEffector, evalCell, evalCells, runCellBody)

import Array exposing (Array)
import Dict exposing (Dict)
import Eval exposing (evalBody, evalTerm, evalTerms)
import EvalStep exposing (EvalBodyStep(..), EvalTermsStep(..), NameSpace, SideEffector, SideEffectorStep(..), Term(..))
import Prelude exposing (BuiltInFn)
import Run exposing (compile, runCount)
import Show exposing (showTerm, showTerms)
import StateGet exposing (getEvalBodyStepState)


type CellState
    = CellState
        { cells : Array (Array Cell)
        , cellstatus : CellStatus
        }


type alias Cell =
    { code : String
    , prog : Result String (List (Term CellState))
    , runstate : RunState
    }


type CellStatus
    = AllGood
    | Blocked Int Int


type RunState
    = RsBlocked (EvalBodyStep CellState) Int Int
    | RsErr String
    | RsOk (Term CellState)


{-| the cell language is schelme plus 'cv'
-}
cellme =
    Prelude.prelude
        |> Dict.union Prelude.math
        |> Dict.insert "cv"
            (TSideEffector (evalArgsPSideEffector cellVal))


evalCell : Array (Array Cell) -> Cell -> Cell
evalCell cells cell =
    let
        prog =
            Run.compile cell.code
    in
    case prog of
        Err _ ->
            { cell | prog = prog }

        Ok p ->
            { cell
                | runstate =
                    runCellBody (EbStart cellme (CellState { cells = cells, cellstatus = AllGood }) p)
            }


{-| should eval all cells, resulting in an updated array, together with
a queue of cells waiting on other cells.
-}
evalCells : Array (Array Cell) -> Array (Array Cell)
evalCells initcells =
    let
        unevaledCells =
            initcells

        {- initcells
           |> Array.map
               (\cellcolumn ->
                   cellcolumn
                       |> Array.map (\cell -> { cell | runstate = RsErr "unevaled" })
                       )
        -}
    in
    unevaledCells
        |> Array.map
            (\cellcolumn ->
                cellcolumn
                    |> Array.map (evalCell unevaledCells)
            )


type PRes
    = PrOk ( NameSpace CellState, CellState, Term CellState )
    | PrPause CellState
    | PrErr String


{-| function type to pass to evalArgsSideEffector
-}
type alias PSideEffectorFn =
    NameSpace CellState -> CellState -> List (Term CellState) -> PRes


runCellBody : EvalBodyStep CellState -> RunState
runCellBody ebs =
    case ebs of
        EbError e ->
            RsErr e

        EbFinal ns state term ->
            RsOk term

        _ ->
            case getEvalBodyStepState ebs of
                Just (CellState s) ->
                    case s.cellstatus of
                        AllGood ->
                            runCellBody (evalBody ebs)

                        Blocked xi yi ->
                            RsBlocked ebs xi yi

                Nothing ->
                    RsErr "error - no step state"


{-| just like the regular evalArgsSideEffector, except checks for PrPause from the fn.
-}
evalArgsPSideEffector : PSideEffectorFn -> SideEffector CellState
evalArgsPSideEffector fn =
    \step ->
        case step of
            SideEffectorStart ns state terms ->
                SideEffectorArgs ns state (evalTerms (EtStart ns state terms))

            SideEffectorArgs ns state ets ->
                case ets of
                    EtFinal efns enstate terms ->
                        -- we have all args, now call our 'built in'
                        case fn ns enstate terms of
                            PrOk ( nebins, nestate, term ) ->
                                SideEffectorFinal nebins nestate term

                            PrErr e ->
                                SideEffectorError e

                            PrPause pstate ->
                                -- result in the same step we are in now, except with updated state.
                                -- caller will need to check for blockage there.
                                SideEffectorArgs ns pstate (EtFinal efns pstate terms)

                    EtError e ->
                        SideEffectorError e

                    _ ->
                        SideEffectorArgs ns state (evalTerms ets)

            SideEffectorEval _ _ _ _ ->
                SideEffectorError "not expecting SideEffectorEval!"

            SideEffectorBody ns state workterms evalstep ->
                SideEffectorError "unexpected SideEffectorBody"

            SideEffectorFinal _ _ _ ->
                step

            SideEffectorError _ ->
                step


cellVal : PSideEffectorFn
cellVal ns (CellState state) args =
    case args of
        [ TNumber x, TNumber y ] ->
            let
                xi =
                    round x

                yi =
                    round y
            in
            Array.get yi state.cells
                |> Maybe.andThen (Array.get xi)
                |> Maybe.map
                    (\cell ->
                        case cell.runstate of
                            RsBlocked rs _ _ ->
                                PrPause <| CellState { state | cellstatus = Blocked xi yi }

                            RsErr e ->
                                PrPause <| CellState { state | cellstatus = Blocked xi yi }

                            RsOk val ->
                                PrOk ( ns, CellState { state | cellstatus = AllGood }, val )
                    )
                |> Maybe.withDefault (PrErr <| "cell not found: " ++ String.fromInt xi ++ ", " ++ String.fromInt yi)

        _ ->
            PrErr (String.concat ("setColor args should be 3 numbers!  " :: List.map showTerm args))
