module Model.Pathfinder.Table.RelatedAddressesTable exposing (ListType(..), Model, filter, totalReceivedColumn)

import Api.Data
import Model.Entity exposing (Entity)
import Model.Graph.Table as Table
import Model.Pathfinder.Id as Pathfinder
import Model.Pathfinder.PagedTable exposing (PagedTable)
import Util.ThemedSelectBox as ThemedSelectBox


type alias Model =
    { table : PagedTable Api.Data.Address
    , entity : Entity
    , addressId : Pathfinder.Id
    , selectBox : ThemedSelectBox.Model ListType
    , selected : ListType
    }


type ListType
    = TaggedAddresses
    | AllAddresses


totalReceivedColumn : String
totalReceivedColumn =
    "Total received"


filter : Table.Filter Api.Data.Address
filter =
    { search = \_ _ -> True
    , filter = always True
    }
