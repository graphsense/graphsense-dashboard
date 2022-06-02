module Init.Graph.Table exposing (..)

import Model.Graph.Table exposing (..)
import Table
import Util.InfiniteScroll as InfiniteScroll


init : String -> Table a
init col =
    { data = []
    , loading = True
    , state = Table.initialSort col
    , nextpage = Nothing
    , infiniteScroll = InfiniteScroll.init
    }
