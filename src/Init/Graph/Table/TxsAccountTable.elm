module Init.Graph.Table.TxsAccountTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)
import Model.Graph.Table.TxsAccountTable exposing (titleTimestamp)


init : Table Api.Data.TxAccount
init =
    Table.initSorted True titleTimestamp
