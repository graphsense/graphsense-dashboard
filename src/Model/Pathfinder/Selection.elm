module Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..))

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
