module Init.Pathfinder.Table.NeighborsTable exposing (init)

import Api.Data
import Components.PagedTable as PagedTable
import Components.Table as Table


init : Int -> PagedTable.Model Api.Data.NeighborAddress
init nrItems =
    PagedTable.init Table.initUnsorted
        |> PagedTable.setNrItems nrItems
        |> PagedTable.setItemsPerPage 5
