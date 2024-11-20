module Model.Graph.Table.AddressTxsUtxoTable exposing (filter)

import Api.Data
import Model.Graph.Table as Table


filter : Table.Filter Api.Data.AddressTxUtxo
filter =
    { search =
        \term a ->
            String.contains term (String.fromInt a.height)
                || String.contains term a.txHash
    , filter = always True
    }
