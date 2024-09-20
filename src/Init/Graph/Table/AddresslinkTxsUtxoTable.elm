module Init.Graph.Table.AddresslinkTxsUtxoTable exposing (init)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.AddresslinkTxsUtxoTable exposing (titleTimestamp)


init : Table Api.Data.LinkUtxo
init =
    Init.Graph.Table.initSorted True titleTimestamp
