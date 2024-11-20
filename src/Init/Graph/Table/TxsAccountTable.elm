module Init.Graph.Table.TxsAccountTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.TxsAccountTable exposing (titleTimestamp)


init : Table Api.Data.TxAccount
init =
    Init.Graph.Table.initSorted True titleTimestamp
