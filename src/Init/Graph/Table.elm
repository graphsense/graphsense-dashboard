module Init.Graph.Table exposing (..)

import Model.Graph.Table exposing (..)
import Table
import Util.InfiniteScroll as InfiniteScroll


init : String -> Table a
init =
    initSorted True


initSorted : Bool -> String -> Table a
initSorted desc col =
    { data = []
    , loading = True
    , state = Table.sortBy col desc
    , nextpage = Nothing
    , infiniteScroll = InfiniteScroll.init
    }
