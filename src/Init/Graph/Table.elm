module Init.Graph.Table exposing (..)

import Model.Graph.Table exposing (..)
import Table


init : String -> Table a
init =
    initSorted True


initSorted : Bool -> String -> Table a
initSorted desc col =
    { data = []
    , filtered = []
    , loading = True
    , state = Table.sortBy col desc
    , nextpage = Nothing
    , searchTerm = Nothing
    }


initUnsorted : Table a
initUnsorted =
    { data = []
    , filtered = []
    , loading = True
    , state = Table.initialSort ""
    , nextpage = Nothing
    , searchTerm = Nothing
    }
