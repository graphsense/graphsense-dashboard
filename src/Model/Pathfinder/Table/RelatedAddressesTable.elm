module Model.Pathfinder.Table.RelatedAddressesTable exposing (ListType(..), Model, filter, getCurrentTable, setCurrentTable, totalReceivedColumn)

import Api.Data
import Model.Entity exposing (Entity)
import Model.Graph.Table as Table
import Model.Pathfinder.Id as Pathfinder
import Model.Pathfinder.PagedTable exposing (PagedTable)
import RecordSetter as Rs
import Util.ThemedSelectBox as ThemedSelectBox


type alias Model =
    { clusterAddresses : PagedTable Api.Data.Address
    , taggedAddresses : PagedTable Api.Data.Address
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


filter : Table.Filter Api.Data.Address
filter =
    { search = \_ _ -> True
    , filter = always True
    }


setCurrentTable : Model -> PagedTable Api.Data.Address -> Model
setCurrentTable ra table =
    case ra.selected of
        TaggedAddresses ->
            Rs.s_taggedAddresses table ra

        ClusterAddresses ->
            Rs.s_clusterAddresses table ra


getCurrentTable : Model -> PagedTable Api.Data.Address
getCurrentTable ra =
    case ra.selected of
        TaggedAddresses ->
            ra.taggedAddresses

        ClusterAddresses ->
            ra.clusterAddresses
