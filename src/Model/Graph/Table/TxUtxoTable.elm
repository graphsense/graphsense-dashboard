module Model.Graph.Table.TxUtxoTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table


titleValue : String
titleValue =
    "Value"


joinAddresses : Api.Data.TxValue -> String
joinAddresses =
    .address >> String.join ","


columnTitleFromDirection : Bool -> String
columnTitleFromDirection isOutgoing =
    (if isOutgoing then
        "Outgoing"

     else
        "Incoming"
    )
        ++ " address"


filter : Table.Filter Api.Data.TxValue
filter =
    { search =
        \term a -> List.any (String.contains term) a.address
    , filter = always True
    }
