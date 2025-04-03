module Model.Pathfinder.Table.RelatedAddressesTable exposing (ListType(..), Model, filter, getCurrentTable, setCurrentTable, totalReceivedColumn)

import Api.Data
import Init.Pathfinder.Id as Pathfinder
import Model.Entity exposing (Entity)
import Model.Graph.Table as Table
import Model.Pathfinder.Id as Pathfinder
import PagedTable
import RecordSetter as Rs
import Util.ThemedSelectBox as ThemedSelectBox


type alias Model =
    { clusterAddresses : PagedTable.Model Api.Data.Address
    , taggedAddresses : PagedTable.Model Api.Data.Address
    , entity : Entity
    , addressId : Pathfinder.Id
    , selectBox : ThemedSelectBox.Model ListType
    , selected : ListType
    }


type ListType
    = TaggedAddresses
    | ClusterAddresses


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


setCurrentTable : Model -> PagedTable.Model Api.Data.Address -> Model
setCurrentTable ra table =
    case ra.selected of
        TaggedAddresses ->
            Rs.s_taggedAddresses table ra

        ClusterAddresses ->
            Rs.s_clusterAddresses table ra


getCurrentTable : Model -> PagedTable.Model Api.Data.Address
getCurrentTable ra =
    case ra.selected of
        TaggedAddresses ->
            ra.taggedAddresses

        ClusterAddresses ->
            ra.clusterAddresses
