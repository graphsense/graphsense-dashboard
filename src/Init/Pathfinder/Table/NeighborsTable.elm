module Init.Pathfinder.Table.NeighborsTable exposing (init)

import Api.Data
import Init.Graph.Table as Table
import PagedTable


init : Int -> PagedTable.Model Api.Data.NeighborAddress
init nrItems =
    PagedTable.init Table.initUnsorted
        |> PagedTable.setNrItems nrItems
        |> PagedTable.setItemsPerPage 5
