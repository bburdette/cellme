module Cellme.DictCellme exposing
    ( CellDict(..)
    , DictCell
    , dictCcr
    , getCd
    , mkCc
    )

{-| implementation of cellme for a Dict of cells with String ids.

@docs CellDict
@docs DictCell
@docs dictCcr
@docs getCd
@docs mkCc

-}

import Cellme.Cellme exposing (CcRecord, Cell, CellContainer(..), CellState)
import Dict exposing (Dict)
import EvalStep exposing (Term(..))
import Show exposing (showTerm)


{-| a Dict from String to Cell.
-}
type CellDict
    = CellDict (Dict String DictCell)


{-| a Cell specialized for CellDict.
-}
type alias DictCell =
    Cell String (CellState String CellDict)


{-| make a CellContainer from a CellDict.
-}
mkCc : CellDict -> CellContainer String CellDict
mkCc mca =
    CellContainer { dictCcr | cells = mca }


{-| get CellDict from a CellContainer.
-}
getCd : CellContainer String CellDict -> CellDict
getCd (CellContainer cc) =
    cc.cells


{-| a CcRecord with functions implemented for CellDict and DictCell.
-}
dictCcr : CcRecord String CellDict
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
                            (Dict.map (\_ v -> fun v) cells)
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
