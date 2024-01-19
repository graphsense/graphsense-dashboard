module Init.Graph.Table.AddressTxsUtxoTable exposing (..)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table, titleTimestamp)


init : Table Api.Data.AddressTxUtxo
init =
    Init.Graph.Table.initSorted True titleTimestamp
