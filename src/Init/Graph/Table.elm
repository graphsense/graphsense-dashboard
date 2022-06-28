module Init.Graph.Table exposing (..)

import Model.Graph.Table exposing (..)
import Table


init : (String -> a -> Bool) -> String -> Table a
init =
    initSorted True


initSorted : Bool -> (String -> a -> Bool) -> String -> Table a
initSorted desc filterFunction col =
    { data = []
    , filtered = []
    , loading = True
    , state = Table.sortBy col desc
    , nextpage = Nothing
    , filter = Nothing
    , filterFunction = filterFunction
    }
