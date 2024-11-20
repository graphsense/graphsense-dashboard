module Init.Graph.Table.LabelAddressTagsTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.LabelAddressTagsTable exposing (titleConfidence)


init : Table Api.Data.AddressTag
init =
    Init.Graph.Table.initSorted True titleConfidence
