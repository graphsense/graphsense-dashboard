module Init.Graph.Table.EntityNeighborsTable exposing (..)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)


init : Table Api.Data.NeighborEntity
init =
    Init.Graph.Table.initUnsorted
