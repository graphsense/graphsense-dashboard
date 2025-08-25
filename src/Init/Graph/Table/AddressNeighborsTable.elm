module Init.Graph.Table.AddressNeighborsTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)


init : Table Api.Data.NeighborAddress
init =
    Table.initUnsorted
