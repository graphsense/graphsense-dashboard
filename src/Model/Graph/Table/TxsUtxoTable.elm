module Model.Graph.Table.TxsUtxoTable exposing (filter, titleNoInputs, titleNoOutputs, titleTotalInput, titleTotalOutput, titleTx)

import Api.Data
import Components.Table as Table


titleTx : String
titleTx =
    "Transaction"


titleNoInputs : String
titleNoInputs =
    "No. inputs"


titleNoOutputs : String
titleNoOutputs =
    "No. outputs"


titleTotalInput : String
titleTotalInput =
    "Total input"


titleTotalOutput : String
titleTotalOutput =
    "Total output"


filter : Table.Filter Api.Data.TxUtxo
filter =
    { search =
        \term a ->
            String.contains term a.txHash
    , filter = always True
    }
