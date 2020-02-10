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
type CellContainer id cc
    = CellContainer
        { cc
            | getCell : id -> CellContainer id cc -> Maybe (Cell id cc)
            , map : (Cell id cc -> Cell id cc) -> CellContainer id cc -> CellContainer id cc
            , has : (Cell id cc -> Bool) -> CellContainer id cc -> Bool

            -- , setCell : id -> Cell id cc -> Result String (CellContainer id)
        }



-- type alias CellArray =
--     { cells : Array (Array Int)
--     , getCell : ( Int, Int ) -> CellContainer CellArray -> Maybe (Cell ( Int, Int ))
--     , map : (Cell ( Int, Int ) -> Cell ( Int, Int )) -> CellContainer CellArray -> CellContainer CellArray
--     , has : (Cell ( Int, Int ) -> Bool) -> CellContainer CellArray -> Bool
--     }


myCellArray : CellContainer ( Int, Int ) { cells : Array (Array (Cell ( Int, Int ))) }
myCellArray =
    CellContainer
        { getCell = \( xi, yi ) (CellContainer cells) -> Array.get yi cells.cells |> Maybe.andThen (Array.get xi)
        , cells = Array.empty
        , map =
            \fun (CellContainer cells) ->
                CellContainer
                    { cells
                        | cells =
                            cells.cells
                                |> Array.map
                                    (\cellcolumn ->
                                        cellcolumn
                                            |> Array.map fun
                                    )
                    }
        , has = \fun (CellContainer cells) -> arraysHas fun cells.cells
        }



-- , cells = Array (Array (Cell ( Int, Int )))
-- (Int, Int) ->  Array (Array Cell) -> Int -> Int -> Maybe Cell
-- getCell icells xi yi =


type CellState id cc
    = CellState
        { cells : CellContainer id cc
        , cellstatus : CellStatus
        }


type alias Cell id cs =
    { code : String
    , prog : Result String (List (Term cs))
    , runstate : RunState id cs
    }


type CellStatus
    = AllGood
    | Blocked Int Int


type RunState id cs
    = RsBlocked (EvalBodyStep cs) id
    | RsErr String
    | RsUnevaled
    | RsOk (Term cs)


{-| the cell language is schelme plus 'cv'
-}
cellme : NameSpace (CellState id cc)
cellme =
    Prelude.prelude
        |> Dict.union Prelude.math
        |> Dict.insert "cv"
            (TSideEffector (evalArgsPSideEffector cellVal))


compileCells : CellContainer id cc -> CellContainer id cc
compileCells cells =
    let
        compileCell =
            \cell -> { cell | prog = Run.compile cell.code }
    in
    cells.map compileCell cells


clearCells : CellContainer id cc -> CellContainer id cc
clearCells cells =
    let
        clearCell =
            \cell -> { cell | runstate = RsUnevaled }
    in
    cells.map clearCell cells


{-| run cell from the start.
-}
runCell : CellContainer id cc -> Cell id cc -> Cell id cc
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
continueCell : CellContainer id cc -> Cell id cc -> Cell id cc
continueCell cells cell =
    case cell.prog of
        Err _ ->
            cell

        Ok _ ->
            case cell.runstate of
                RsBlocked cb _ ->
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


arraysHas : (a -> Bool) -> Array (Array a) -> Bool
arraysHas condf arrays =
    arrays
        |> arrayHas
            (\cc ->
                cc
                    |> arrayHas condf
            )


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


compileError : CellContainer id cc -> Bool
compileError cells =
    cells.has
        (\cell ->
            case cell.prog of
                Err _ ->
                    True

                _ ->
                    False
        )
        cells.cells


isLoopedCell : CellContainer id cc -> List id -> Cell id cc -> Maybe (List id)
isLoopedCell cells loop cell =
    case cell.runstate of
        RsBlocked _ id ->
            if List.member id loop then
                Just loop

            else
                cells.getCell cells id |> Maybe.andThen (isLoopedCell cells (id :: loop))

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


errorCheck : CellContainer id cc -> Bool
errorCheck cells =
    cells.has
        (\cell ->
            case cell.runstate of
                RsErr _ ->
                    True

                _ ->
                    False
        )
        cells.cells


loopCheck : CellContainer id cc -> Bool
loopCheck cells =
    cells.has (\cell -> isLoopedCell cells [] cell |> maybeIsJust) cells.cells


{-| should eval all cells, resulting in an updated array and result type.
-}
evalCellsFully : CellContainer id cc -> ( CellContainer id cc, FullEvalResult )
evalCellsFully (CellContainer initcells) =
    let
        compiledCells =
            CellContainer initcells
                |> compileCells
                |> clearCells
    in
    if compileError compiledCells then
        ( compiledCells, FeCompileError )

    else
        compiledCells
            |> initcells.map (runCell compiledCells)
            |> runCellsFully


runCellsFully : CellContainer id cc -> ( CellContainer id cc, FullEvalResult )
runCellsFully (CellContainer cells) =
    if loopCheck (CellContainer cells) then
        ( cells, FeLoop )

    else if errorCheck (CellContainer cells) then
        ( cells, FeEvalError )

    else if
        cells.has
            (\cell ->
                case cell.runstate of
                    RsBlocked _ _ ->
                        True

                    RsUnevaled ->
                        False

                    RsErr _ ->
                        False

                    RsOk _ ->
                        False
            )
            (CellContainer cells)
    then
        runCellsFully (cells.map (continueCell (CellContainer cells)) cells.cells)

    else
        ( CellContainer cells, FeOk )


evalCell : CellContainer id cc -> Cell id cc -> Cell id cc
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
evalCellsOnce : CellContainer id cc -> CellContainer id cc
evalCellsOnce initcells =
    let
        unevaledCells =
            initcells
    in
    initcells.map (evalCell unevaledCells) unevaledCells


type PRes id cc
    = PrOk ( NameSpace (CellState id cc), CellState id cc, Term (CellState id cc) )
    | PrPause (CellState id cc)
    | PrErr String


{-| function type to pass to evalArgsSideEffector
-}
type alias PSideEffectorFn id cc =
    NameSpace (CellState id cc) -> CellState id cc -> List (Term (CellState id cc)) -> PRes id cc


runCellBody : EvalBodyStep (CellState id cc) -> RunState id cc
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
evalArgsPSideEffector : PSideEffectorFn id cc -> SideEffector (CellState id cc)
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


cellVal : PSideEffectorFn id cc
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
                            RsBlocked _ _ ->
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
