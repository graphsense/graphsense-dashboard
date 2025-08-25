module Model.Pathfinder.Table.RelatedAddressesPubkeyTable exposing (Model, filter, getTable, hasData, init, setTable)

import Api.Data
import Init.Graph.Table as Table
import Init.Pathfinder.Id as Pathfinder
import Model.Graph.Table as Table
import Model.Pathfinder.Id exposing (Id)
import PagedTable
import RecordSetter as Rs


type alias Model =
    { table : PagedTable.Model Api.Data.RelatedAddress
    , addressId : Id
    }


init : Id -> Model
init addressId =
    { table =
        PagedTable.init Table.initUnsorted
            |> PagedTable.setItemsPerPage 5
    , addressId = addressId
    }


filter : Model -> Table.Filter Api.Data.RelatedAddress
filter { addressId } =
    { search = \_ _ -> True
    , filter =
        \address ->
            Pathfinder.init address.currency address.address /= addressId
    }


setTable : Model -> PagedTable.Model Api.Data.RelatedAddress -> Model
setTable ra table =
    Rs.s_table table ra


getTable : Model -> PagedTable.Model Api.Data.RelatedAddress
getTable ra =
    ra.table


hasData : Model -> Bool
hasData ra =
    PagedTable.hasData ra.table
