module Init.Graph.Table.EntityNeighborsTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)


init : Table Api.Data.NeighborEntity
init =
    Table.initUnsorted
