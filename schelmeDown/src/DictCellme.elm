module DictCellme exposing (CellDict(..), DictCell, dictCc, dictCcr, getCd, listHas, mkCc)

import Cellme exposing (Cell, CellContainer(..), CellState)
import Dict exposing (Dict)
import EvalStep exposing (Term(..))
import Show exposing (showTerm, showTerms)


type CellDict
    = CellDict (Dict String DictCell)


type alias DictCell =
    Cell String (CellState String CellDict)


mkCc : CellDict -> CellContainer String CellDict
mkCc mca =
    let
        (CellContainer cc) =
            dictCc
    in
    CellContainer { cc | cells = mca }


getCd : CellContainer String CellDict -> CellDict
getCd (CellContainer cc) =
    cc.cells


dictCc : CellContainer String CellDict
dictCc =
    CellContainer dictCcr


dictCcr =
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
                    Err (String.concat ("cv arg should be a string (cell name)!  " :: List.map showTerm args))
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
