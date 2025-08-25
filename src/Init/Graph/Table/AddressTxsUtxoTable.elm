module Init.Graph.Table.AddressTxsUtxoTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)
import Model.Graph.Table exposing (titleTimestamp)


init : Table Api.Data.AddressTxUtxo
init =
    Table.initSorted True titleTimestamp
