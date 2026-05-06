module Init.Pathfinder.Table.TagsTable exposing (init, pagesize)

import Api.Data
import Components.InfiniteTable as InfiniteTable


pagesize : Int
pagesize =
    100


init : String -> InfiniteTable.Model Api.Data.AddressTag
init tableId =
    InfiniteTable.init tableId pagesize
