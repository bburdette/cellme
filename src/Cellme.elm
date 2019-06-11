module Cellme exposing (Cell, CellState(..), CellStatus(..), FullEvalResult(..), PRes(..), PSideEffectorFn, RunState(..), arrayFirst, arrayHas, cellVal, cellme, compileCells, compileError, continueCell, evalArgsPSideEffector, evalCell, evalCellsFully, evalCellsOnce, isLoopedCell, loopCheck, maybeIsJust, runCell, runCellBody)

import Array exposing (Array)
import Dict exposing (Dict)
import Eval exposing (evalBody, evalTerm, evalTerms)
import EvalStep exposing (EvalBodyStep(..), EvalTermsStep(..), NameSpace, SideEffector, SideEffectorStep(..), Term(..))
import Prelude exposing (BuiltInFn)
import Run exposing (compile, runCount)
import Show exposing (showTerm, showTerms)
import StateGet exposing (getEvalBodyStepState)
import StateSet exposing (setEvalBodyStepState)


{-| the state that is used during cell eval.
-}
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
    | RsUnevaled
    | RsOk (Term CellState)


getCell : Array (Array Cell) -> Int -> Int -> Maybe Cell
getCell cells xi yi =
    Array.get yi cells |> Maybe.andThen (Array.get xi)


{-| the cell language is schelme plus 'cv'
-}
cellme : NameSpace CellState
cellme =
    Prelude.prelude
        |> Dict.union Prelude.math
        |> Dict.insert "cv"
            (TSideEffector (evalArgsPSideEffector cellVal))


compileCells : Array (Array Cell) -> Array (Array Cell)
compileCells cells =
    let
        compileCell =
            \cell -> { cell | prog = Run.compile cell.code }
    in
    cells
        |> Array.map
            (\cellcolumn ->
                cellcolumn
                    |> Array.map compileCell
            )


clearCells : Array (Array Cell) -> Array (Array Cell)
clearCells cells =
    let
        clearCell =
            \cell -> { cell | runstate = RsUnevaled }
    in
    cells
        |> Array.map
            (\cellcolumn ->
                cellcolumn
                    |> Array.map clearCell
            )


{-| run cell from the start.
-}
runCell : Array (Array Cell) -> Cell -> Cell
runCell cells cell =
    case cell.prog of
        Err _ ->
            cell

        Ok p ->
            { cell
                | runstate =
                    runCellBody (EbStart cellme (CellState { cells = cells, cellstatus = AllGood }) p)
            }


{-| continue running the cell.
-}
continueCell : Array (Array Cell) -> Cell -> Cell
continueCell cells cell =
    case cell.prog of
        Err _ ->
            cell

        Ok _ ->
            case cell.runstate of
                RsBlocked cb _ _ ->
                    { cell
                        | runstate = runCellBody (setEvalBodyStepState cb (CellState { cells = cells, cellstatus = AllGood }))
                    }

                _ ->
                    cell


type FullEvalResult
    = FeOk
    | FeLoop
    | FeEvalError
    | FeCompileError


arrayHas : (a -> Bool) -> Array a -> Bool
arrayHas condf array =
    let
        arrayHasHelper : Int -> Bool
        arrayHasHelper idx =
            case
                Array.get idx array
                    |> Maybe.map condf
            of
                Nothing ->
                    False

                Just b ->
                    if b then
                        True

                    else
                        arrayHasHelper (idx + 1)
    in
    arrayHasHelper 0


arrayFirst : (a -> Maybe b) -> Array a -> Maybe b
arrayFirst condf array =
    let
        arrayFirstHelper : Int -> Maybe b
        arrayFirstHelper idx =
            Array.get idx array
                |> Maybe.andThen
                    (\a ->
                        case condf a of
                            Nothing ->
                                arrayFirstHelper (idx + 1)

                            Just b ->
                                Just b
                    )
    in
    arrayFirstHelper 0


compileError : Array (Array Cell) -> Bool
compileError array =
    array
        |> arrayHas
            (\cc ->
                cc
                    |> arrayHas
                        (\cell ->
                            case cell.prog of
                                Err _ ->
                                    True

                                _ ->
                                    False
                        )
            )


isLoopedCell : Array (Array Cell) -> List ( Int, Int ) -> Cell -> Maybe (List ( Int, Int ))
isLoopedCell cells loop cell =
    case cell.runstate of
        RsBlocked _ xi yi ->
            if List.member ( xi, yi ) loop then
                Just loop

            else
                getCell cells xi yi |> Maybe.andThen (isLoopedCell cells (( xi, yi ) :: loop))

        RsUnevaled ->
            Nothing

        RsErr _ ->
            Nothing

        RsOk _ ->
            Nothing


maybeIsJust : Maybe a -> Bool
maybeIsJust mba =
    case mba of
        Just _ ->
            True

        Nothing ->
            False


errorCheck : Array (Array Cell) -> Bool
errorCheck cells =
    cells
        |> arrayHas
            (arrayHas
                (\cell ->
                    case cell.runstate of
                        RsErr _ ->
                            True

                        _ ->
                            False
                )
            )


loopCheck : Array (Array Cell) -> Bool
loopCheck cells =
    cells
        |> arrayHas (arrayHas (\cell -> isLoopedCell cells [] cell |> maybeIsJust))


{-| should eval all cells, resulting in an updated array and result type.
-}
evalCellsFully : Array (Array Cell) -> ( Array (Array Cell), FullEvalResult )
evalCellsFully initcells =
    let
        compiledCells =
            initcells
                |> compileCells
                |> clearCells
    in
    if compileError compiledCells then
        ( compiledCells, FeCompileError )

    else
        compiledCells
            |> Array.map (Array.map (runCell compiledCells))
            |> runCellsFully


runCellsFully : Array (Array Cell) -> ( Array (Array Cell), FullEvalResult )
runCellsFully cells =
    if loopCheck cells then
        ( cells, FeLoop )

    else if errorCheck cells then
        ( cells, FeEvalError )

    else if
        cells
            |> arrayHas
                (arrayHas
                    (\cell ->
                        case cell.runstate of
                            RsBlocked _ _ _ ->
                                True

                            RsUnevaled ->
                                False

                            RsErr _ ->
                                False

                            RsOk _ ->
                                False
                    )
                )
    then
        runCellsFully (Array.map (Array.map (continueCell cells)) cells)

    else
        ( cells, FeOk )


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


{-| should eval all cells, resulting in an updated array.
-}
evalCellsOnce : Array (Array Cell) -> Array (Array Cell)
evalCellsOnce initcells =
    let
        unevaledCells =
            initcells
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

        EbFinal _ _ term ->
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

            SideEffectorBody _ _ _ _ ->
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
                            RsBlocked _ _ _ ->
                                PrPause <| CellState { state | cellstatus = Blocked xi yi }

                            RsErr _ ->
                                PrErr <| "Blocked on error in : " ++ String.fromInt xi ++ ", " ++ String.fromInt yi

                            RsUnevaled ->
                                PrPause <| CellState { state | cellstatus = Blocked xi yi }

                            RsOk val ->
                                PrOk ( ns, CellState { state | cellstatus = AllGood }, val )
                    )
                |> Maybe.withDefault (PrErr <| "cell not found: " ++ String.fromInt xi ++ ", " ++ String.fromInt yi)

        _ ->
            PrErr (String.concat ("cv args should be 2 numbers!  " :: List.map showTerm args))
