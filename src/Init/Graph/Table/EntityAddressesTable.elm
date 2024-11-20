module Init.Graph.Table.EntityAddressesTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)


init : Table Api.Data.Address
init =
    Init.Graph.Table.initUnsorted
