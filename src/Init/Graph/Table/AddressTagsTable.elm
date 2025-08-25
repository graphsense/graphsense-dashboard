module Init.Graph.Table.AddressTagsTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)


init : Table Api.Data.AddressTag
init =
    Table.initUnsorted
