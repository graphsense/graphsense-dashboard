module Model.Graph.Table.AddressTxsUtxoTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table


titleTx : String
titleTx =
    "Transaction"


titleValue : String
titleValue =
    "Value"


titleHeight : String
titleHeight =
    "Height"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


filter : Table.Filter Api.Data.AddressTxUtxo
filter =
    { search =
        \term a ->
            String.contains term (String.fromInt a.height)
                || String.contains term a.txHash
    , filter = always True
    }
