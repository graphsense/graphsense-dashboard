module Model.Graph.Table.TxsAccountTable exposing (filter, titleHeight, titleReceivingAddress, titleSendingAddress, titleTimestamp, titleTx)

import Api.Data
import Config.Graph as Graph
import Model.Currency exposing (assetFromBase)
import Model.Graph.Table as Table
import Util.Graph as Graph


titleTx : String
titleTx =
    "Transaction"


titleHeight : String
titleHeight =
    "Height"


titleTimestamp : String
titleTimestamp =
    "Timestamp"


titleSendingAddress : String
titleSendingAddress =
    "Sending address"


titleReceivingAddress : String
titleReceivingAddress =
    "Receiving address"


filter : Graph.Config -> Table.Filter Api.Data.TxAccount
filter gc =
    { search =
        \term a ->
            String.contains term a.txHash
                || String.contains term (String.fromInt a.height)
                || String.contains term a.fromAddress
                || String.contains term a.toAddress
                || String.contains (String.toLower term) (String.toLower a.currency)
    , filter =
        \a ->
            Graph.filterTxValue gc a.currency a.value Nothing
    }
