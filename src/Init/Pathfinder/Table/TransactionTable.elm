module Init.Pathfinder.Table.TransactionTable exposing (..)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Pathfinder.Table.TransactionTable exposing (titleTimestamp)


init : Table Api.Data.AddressTx
init =
    Init.Graph.Table.initSorted True titleTimestamp
