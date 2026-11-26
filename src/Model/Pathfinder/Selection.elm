module Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..), toMultiSelect)

import Model.Pathfinder.Id exposing (Id)


type Selection
    = SelectedAddress Id
    | SelectedTx Id
    | SelectedAggEdge ( Id, Id )
    | MultiSelect (List MultiSelectOptions)
    | WillSelectTx Id
    | WillSelectAddress Id
    | WillSelectAggEdge ( Id, Id )
    | SelectedConversionEdge ( Id, Id )
    | NoSelection


type MultiSelectOptions
    = MSelectedAddress Id
    | MSelectedTx Id


{-| The nodes a selection covers, as a multi-selection: an agg edge stands
for its two addresses. Pending (Will...) and conversion-edge selections cover
no node.
-}
toMultiSelect : Selection -> List MultiSelectOptions
toMultiSelect selection =
    case selection of
        SelectedAddress id ->
            [ MSelectedAddress id ]

        SelectedTx id ->
            [ MSelectedTx id ]

        SelectedAggEdge ( a, b ) ->
            [ MSelectedAddress a, MSelectedAddress b ]

        MultiSelect sel ->
            sel

        _ ->
            []
