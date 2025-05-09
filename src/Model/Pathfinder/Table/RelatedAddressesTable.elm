module Model.Pathfinder.Table.RelatedAddressesTable exposing (Model, filter, getTable, setTable, totalReceivedColumn)

import Api.Data
import Init.Pathfinder.Id as Pathfinder
import Model.Entity exposing (Entity)
import Model.Graph.Table as Table
import Model.Pathfinder.Id as Pathfinder
import PagedTable
import RecordSetter as Rs
import Set exposing (Set)


type alias Model =
    { table : PagedTable.Model Api.Data.Address
    , entity : Entity
    , addressId : Pathfinder.Id
    , existingTaggedAddresses : Set String
    , allTaggedAddressesFetched : Bool
    }


totalReceivedColumn : String
totalReceivedColumn =
    "Total received"


filter : Model -> Table.Filter Api.Data.Address
filter { addressId } =
    { search = \_ _ -> True
    , filter =
        \address ->
            Pathfinder.init address.currency address.address /= addressId
    }


setTable : Model -> PagedTable.Model Api.Data.Address -> Model
setTable ra table =
    Rs.s_table table ra


getTable : Model -> PagedTable.Model Api.Data.Address
getTable ra =
    ra.table
