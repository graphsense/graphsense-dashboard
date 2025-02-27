module Model.Pathfinder.Table.RelatedAddressesTable exposing (Model, filter, totalReceivedColumn)

import Api.Data
import Model.Graph.Table as Table
import Model.Pathfinder.PagedTable exposing (PagedTable)


type alias Model =
    { table : PagedTable Api.Data.Address
    }


totalReceivedColumn : String
totalReceivedColumn =
    "Total received"


filter : Table.Filter Api.Data.Address
filter =
    { search = \_ _ -> True
    , filter = always True
    }
