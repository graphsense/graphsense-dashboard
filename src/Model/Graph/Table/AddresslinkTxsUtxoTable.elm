module Model.Graph.Table.AddresslinkTxsUtxoTable exposing (filter, titleHeight, titleInputValue, titleOutputValue, titleTimestamp, titleTx)

import Api.Data
import Components.Table as Table


titleTx : String
titleTx =
    "Transaction"


titleInputValue : String
titleInputValue =
    "Input value"


titleOutputValue : String
titleOutputValue =
    "Output value"


titleHeight : String
titleHeight =
    "Height"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


filter : Table.Filter Api.Data.LinkUtxo
filter =
    { search =
        \term a ->
            String.contains term a.txHash
                || String.contains term (String.fromInt a.height)
    , filter = always True
    }
