module Init.Graph.Table.AddressNeighborsTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)


init : Table Api.Data.NeighborAddress
init =
    Init.Graph.Table.initUnsorted
