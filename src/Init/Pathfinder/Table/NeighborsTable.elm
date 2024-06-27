module Init.Pathfinder.Table.NeighborsTable exposing (..)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Pathfinder.Table exposing (PagedTable)


init : Int -> PagedTable Api.Data.NeighborAddress
init nrItems =
    let
        m =
            Init.Graph.Table.initUnsorted
    in
    { table = m
    , nrItems = Just nrItems
    , currentPage = 1
    , itemsPerPage = 5
    }
