module Init.Pathfinder.Table.TagsTable exposing (init, pagesize)

import Api.Data
import Components.InfiniteTable as InfiniteTable
import Components.Table as Table


pagesize : Int
pagesize =
    100


init : String -> InfiniteTable.Model Api.Data.AddressTag
init tableId =
    Table.initUnsorted
        |> InfiniteTable.init tableId pagesize
