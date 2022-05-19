module Init.Graph.Browser exposing (..)

import InfiniteList
import Model.Graph.Browser exposing (..)
import Time


init : Int -> Model
init now =
    { visible = False
    , type_ = None
    , now = Time.millisToPosix now
    , table = NoTable
    }


initTable : Table a
initTable =
    { data = []
    , loading = True
    , table = InfiniteList.init
    , nextpage = Nothing
    }
