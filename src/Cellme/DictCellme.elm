module Cellme.DictCellme exposing
    ( CellDict(..)
    , DictCell
    , dictCc
    , dictCcr
    , getCd
    , mkCc
    )

{-| implementation of CellContainer for a Dict String Cell. (cv "name") -> Cell.

@docs CellDict
@docs DictCell
@docs dictCc
@docs dictCcr
@docs getCd
@docs mkCc

-}

import Cellme.Cellme exposing (CcRecord, Cell, CellContainer(..), CellState)
import Dict exposing (Dict)
import EvalStep exposing (Term(..))
import Show exposing (showTerm)


{-| CellDict type contains a Dict from String to Cell.
-}
type CellDict
    = CellDict (Dict String DictCell)


{-| DictCell contains a program that has a CellDict as part of its state.
-}
type alias DictCell =
    Cell String (CellState String CellDict)


{-| make CellContainer.
-}
mkCc : CellDict -> CellContainer String CellDict
mkCc mca =
    let
        (CellContainer cc) =
            dictCc
    in
    CellContainer { cc | cells = mca }


{-| get CellDict from the CellContainer..
-}
getCd : CellContainer String CellDict -> CellDict
getCd (CellContainer cc) =
    cc.cells


{-| a CellDict CellContainer with an empty CellDict.
-}
dictCc : CellContainer String CellDict
dictCc =
    CellContainer dictCcr


{-| the record used to build a CellDict CellContainer faux-typeclass.
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
