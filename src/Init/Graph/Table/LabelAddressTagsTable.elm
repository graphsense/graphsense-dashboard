module Init.Graph.Table.LabelAddressTagsTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)
import Model.Graph.Table.LabelAddressTagsTable exposing (titleConfidence)


init : Table Api.Data.AddressTag
init =
    Table.initSorted True titleConfidence
