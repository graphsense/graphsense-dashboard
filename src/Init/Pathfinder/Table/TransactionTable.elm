module Init.Pathfinder.Table.TransactionTable exposing (..)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Pathfinder.Table exposing (PagedTable)


init : Maybe Int -> PagedTable Api.Data.AddressTx
init nrItems =
    let
        m =
            Init.Graph.Table.initUnsorted
    in
    { table = m
    , nrItems = nrItems
    , currentPage = 1
    , itemsPerPage = 5
    }
