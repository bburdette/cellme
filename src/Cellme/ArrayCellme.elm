module Cellme.ArrayCellme exposing
    ( CellArray(..)
    , ArrayCell
    , arrayCcr
    , getCa
    , mkCc
    )

{-| implementation of cellme for a 2D Array of cells - spreadsheet style.

@docs CellArray
@docs ArrayCell
@docs arrayCcr
@docs getCa
@docs mkCc

-}

import Array exposing (Array)
import Cellme.Cellme exposing (CcRecord, Cell, CellContainer(..), CellState)
import EvalStep exposing (Term(..))
import Show exposing (showTerm, showTerms)


{-| a 2D array of Cells.
-}
type CellArray
    = CellArray (Array (Array ArrayCell))


{-| a Cell with a CellArray id and CellArray in its run state.
-}
type alias ArrayCell =
    Cell ( Int, Int ) (CellState ( Int, Int ) CellArray)


{-| make a CellContainer from a CellArray.
-}
mkCc : CellArray -> CellContainer ( Int, Int ) CellArray
mkCc mca =
    CellContainer { arrayCcr | cells = mca }


{-| get the CellArray from a CellContainer.
-}
getCa : CellContainer ( Int, Int ) CellArray -> CellArray
getCa (CellContainer cc) =
    cc.cells


{-| a CcRecord with functions implemented for CellArray and ArrayCell.
-}
arrayCcr : CcRecord ( Int, Int ) CellArray
arrayCcr =
    { getCell =
        \( xi, yi ) (CellContainer cells) ->
            let
                (CellArray mca) =
                    cells.cells
            in
            Array.get yi mca |> Maybe.andThen (Array.get xi)
    , setCell =
        \( xi, yi ) cell (CellContainer cells) ->
            let
                (CellArray mca) =
                    cells.cells
            in
            Array.get yi mca
                |> Maybe.map
                    (\rowarray ->
                        Array.set yi (Array.set xi cell rowarray) mca
                    )
                |> Result.fromMaybe ("invalid key: (" ++ String.fromInt xi ++ ", " ++ String.fromInt xi ++ ")")
                |> Result.map (\newcells -> CellContainer { cells | cells = CellArray newcells })
    , cells = CellArray Array.empty
    , map =
        \fun (CellContainer cellz) ->
            let
                (CellArray cells) =
                    cellz.cells
            in
            CellContainer
                { cellz
                    | cells =
                        CellArray
                            (cells
                                |> Array.map
                                    (\cellcolumn ->
                                        cellcolumn
                                            |> Array.map fun
                                    )
                            )
                }
    , has =
        \fun (CellContainer cells) ->
            let
                (CellArray mca) =
                    cells.cells
            in
            arraysHas fun mca
    , makeId =
        \args ->
            case args of
                [ TNumber x, TNumber y ] ->
                    let
                        xi =
                            round x

                        yi =
                            round y
                    in
                    Ok
                        ( xi, yi )

                _ ->
                    Err (String.concat ("cv args should be 2 numbers!  " :: List.map showTerm args))
    , showId = \( xi, yi ) -> String.concat [ "(", String.fromInt xi, ", ", String.fromInt yi, ")" ]
    }


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
