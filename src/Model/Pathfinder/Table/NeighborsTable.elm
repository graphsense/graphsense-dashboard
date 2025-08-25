module Model.Pathfinder.Table.NeighborsTable exposing (filter)

import Api.Data
import Components.Table as Table


filter : Table.Filter Api.Data.NeighborAddress
filter =
    { search =
        \term a ->
            String.contains term a.address.address
    , filter = always True
    }
