module Init.Graph.Table.UserAddressTagsTable exposing (..)

import Init.Graph.Table
import Model.Graph.Table exposing (Table, titleLabel)
import Model.Graph.Tag as Tag
import RecordSetter exposing (..)


init : Table Tag.UserTag
init =
    Init.Graph.Table.initSorted True titleLabel
        |> s_loading False
