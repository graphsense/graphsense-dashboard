module Init.Graph.Table.UserAddressTagsTable exposing (init)

import Components.Table as Table exposing (Table)
import Model.Graph.Table.UserAddressTagsTable exposing (titleLabel)
import Model.Graph.Tag as Tag
import RecordSetter exposing (..)


init : Table Tag.UserTag
init =
    Table.initSorted True titleLabel
        |> s_loading False
