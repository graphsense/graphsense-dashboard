module Init.Graph.Table exposing (..)

import Model.Graph.Table exposing (..)
import Table


init : Table a
init =
    { data = []
    , loading = True
    , state = Table.initialSort "transaction"
    , nextpage = Nothing
    }
