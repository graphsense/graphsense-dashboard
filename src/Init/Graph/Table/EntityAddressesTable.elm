module Init.Graph.Table.EntityAddressesTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)


init : Table Api.Data.Address
init =
    Table.initUnsorted
