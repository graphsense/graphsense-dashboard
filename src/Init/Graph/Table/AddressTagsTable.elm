module Init.Graph.Table.AddressTagsTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)


init : Table Api.Data.AddressTag
init =
    Init.Graph.Table.initUnsorted
