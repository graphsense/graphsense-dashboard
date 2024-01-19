module Init.Graph.Table.AllAssetsTable exposing (..)

import Init.Graph.Table
import Model.Graph.Table exposing (AllAssetsTable, titleValue)
import RecordSetter exposing (..)


init : AllAssetsTable
init =
    Init.Graph.Table.initSorted True titleValue
        |> s_loading False
