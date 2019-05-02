module Cellme exposing (Cell, CellState(..), CellStatus(..), PRes(..), PSideEffectorFn, RunState(..), cellVal, evalArgsPSideEffector, sheetlang)

import Array exposing (Array)
import Dict exposing (Dict)
import Eval exposing (evalBody, evalTerm, evalTerms)
import EvalStep exposing (EvalTermsStep(..), NameSpace, SideEffector, SideEffectorStep(..), Term(..))
import Prelude exposing (BuiltInFn)
import Run exposing (compile, runCount)
import Show exposing (showTerm, showTerms)


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
    = RsRunning (EvalTermsStep CellState)
    | RsErr String
    | RsOk (Term CellState)



-- ok make a special sideeffector evaluator that can result in paused execution.


{-| function type to pass to evalArgsSideEffector
-}
type PRes a
    = PrOk ( NameSpace a, a, Term a )
    | PrPause a
    | PrErr String


type alias PSideEffectorFn a =
    NameSpace a -> a -> List (Term a) -> PRes a


{-| make a SideEffector function where arguments are evaled before the SideEffectorFn function is called.
-}
evalArgsPSideEffector : PSideEffectorFn a -> SideEffector a
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


sheetlang =
    Prelude.prelude
        |> Dict.union Prelude.math
        |> Dict.insert "cellVal"
            (TSideEffector (evalArgsPSideEffector cellVal))


cellVal : PSideEffectorFn CellState
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
                            RsRunning rs ->
                                PrPause <| CellState { state | cellstatus = Blocked xi yi }

                            RsErr e ->
                                PrErr e

                            RsOk val ->
                                PrOk ( ns, CellState { state | cellstatus = AllGood }, val )
                    )
                |> Maybe.withDefault (PrErr <| "cell not found: " ++ String.fromInt xi ++ ", " ++ String.fromInt yi)

        _ ->
            PrErr (String.concat ("setColor args should be 3 numbers!  " :: List.map showTerm args))
