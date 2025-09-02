module Init.Pathfinder.Table.NeighborsTable exposing (init)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table


init : Bool -> Int -> InfiniteTable.Model Api.Data.NeighborAddress
init isOutgoing _ =
    let
        dir =
            if isOutgoing then
                "outgoing"

            else
                "incoming"
    in
    InfiniteTable.init ("neighborsTable_" ++ dir) 25 Table.initUnsorted
