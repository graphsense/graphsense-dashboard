module Init.Graph.Table.AddresslinkTxsUtxoTable exposing (init)

import Api.Data
import Components.Table as Table exposing (Table)
import Model.Graph.Table.AddresslinkTxsUtxoTable exposing (titleTimestamp)


init : Table Api.Data.LinkUtxo
init =
    Table.initSorted True titleTimestamp
