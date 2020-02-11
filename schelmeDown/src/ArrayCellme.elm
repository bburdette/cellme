module ArrayCellme exposing (MyCell, MyCellArray(..), getMca, mkMca, myCellArray)

import Array exposing (Array)
import Cellme exposing (Cell, CellContainer(..), CellState)
import EvalStep exposing (Term(..))
import Show exposing (showTerm, showTerms)


type MyCellArray
    = MyCellArray (Array (Array (Cell ( Int, Int ) (CellState ( Int, Int ) MyCellArray))))


type alias MyCell =
    Cell ( Int, Int ) (CellState ( Int, Int ) MyCellArray)


mkMca : MyCellArray -> CellContainer ( Int, Int ) MyCellArray
mkMca mca =
    let
        (CellContainer cc) =
            myCellArray
    in
    CellContainer { cc | cells = mca }


getMca : CellContainer ( Int, Int ) MyCellArray -> MyCellArray
getMca (CellContainer cc) =
    cc.cells


myCellArray : CellContainer ( Int, Int ) MyCellArray
myCellArray =
    CellContainer
        { getCell =
            \( xi, yi ) (CellContainer cells) ->
                let
                    (MyCellArray mca) =
                        cells.cells
                in
                Array.get yi mca |> Maybe.andThen (Array.get xi)
        , setCell =
            \( xi, yi ) cell (CellContainer cells) ->
                let
                    (MyCellArray mca) =
                        cells.cells
                in
                Array.get yi mca
                    |> Maybe.map
                        (\rowarray ->
                            Array.set yi (Array.set xi cell rowarray) mca
                        )
                    |> Result.fromMaybe ("invalid key: (" ++ String.fromInt xi ++ ", " ++ String.fromInt xi ++ ")")
                    |> Result.map (\newcells -> CellContainer { cells | cells = MyCellArray newcells })
        , cells = MyCellArray Array.empty
        , map =
            \fun (CellContainer cellz) ->
                let
                    (MyCellArray cells) =
                        cellz.cells
                in
                CellContainer
                    { cellz
                        | cells =
                            MyCellArray
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
                    (MyCellArray mca) =
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
