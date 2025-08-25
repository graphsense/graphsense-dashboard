module Init.Graph.Table.AllAssetsTable exposing (init)

import Components.Table as Table
import Model.Graph.Table exposing (AllAssetsTable, titleValue)
import RecordSetter exposing (..)


init : AllAssetsTable
init =
    Table.initSorted True titleValue
        |> s_loading False
