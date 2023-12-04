module Init.Graph.Table.TxsUtxoTable exposing (..)

import Api.Data
import Init.Graph.Table
import Model.Graph.Table exposing (Table)


init : Table Api.Data.TxUtxo
init =
    Init.Graph.Table.initUnsorted
