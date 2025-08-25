module Init.Pathfinder.Table.NeighborsTable exposing (init)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table


init : Int -> InfiniteTable.Model Api.Data.NeighborAddress
init _ =
    InfiniteTable.init 25 Table.initUnsorted
