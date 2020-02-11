module DictCellme exposing (CellDict(..), DictCell, getMca, listHas, mkCc, myCellDict)

import Cellme exposing (Cell, CellContainer(..), CellState)
import Dict exposing (Dict)
import EvalStep exposing (Term(..))
import Show exposing (showTerm, showTerms)


type CellDict
    = CellDict (Dict String (Cell String (CellState String CellDict)))


type alias DictCell =
    Cell String (CellState String CellDict)


mkCc : CellDict -> CellContainer String CellDict
mkCc mca =
    let
        (CellContainer cc) =
            myCellDict
    in
    CellContainer { cc | cells = mca }


getMca : CellContainer String CellDict -> CellDict
getMca (CellContainer cc) =
    cc.cells


myCellDict : CellContainer String CellDict
myCellDict =
    CellContainer
        { getCell =
            \key (CellContainer cells) ->
                let
                    (CellDict mca) =
                        cells.cells
                in
                Dict.get key mca
        , setCell =
            \key cell (CellContainer cells) ->
                let
                    (CellDict mca) =
                        cells.cells
                in
                Ok <| CellContainer { cells | cells = CellDict (Dict.insert key cell mca) }
        , cells = CellDict Dict.empty
        , map =
            \fun (CellContainer cellz) ->
                let
                    (CellDict cells) =
                        cellz.cells
                in
                CellContainer
                    { cellz
                        | cells =
                            CellDict
                                (Dict.map (\k v -> fun v) cells)
                    }
        , has =
            \fun (CellContainer cells) ->
                let
                    (CellDict mca) =
                        cells.cells

                    l =
                        Dict.values mca
                in
                listHas fun l
        , makeId =
            \args ->
                case args of
                    [ TString s ] ->
                        Ok s

                    _ ->
                        Err (String.concat ("cv args should be 2 numbers!  " :: List.map showTerm args))
        , showId = identity
        }


listHas : (a -> Bool) -> List a -> Bool
listHas condf list =
    case list of
        a :: rest ->
            if condf a then
                True

            else
                listHas condf rest

        [] ->
            False
