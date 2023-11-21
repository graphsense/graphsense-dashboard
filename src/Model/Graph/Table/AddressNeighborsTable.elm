module Model.Graph.Table.AddressNeighborsTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table


titleLabels : String
titleLabels =
    "Tags"


titleAddressBalance : String
titleAddressBalance =
    "Address balance"


titleAddressReceived : String
titleAddressReceived =
    "Address received"


titleNoTxs : String
titleNoTxs =
    "No. transactions"


titleEstimatedValue : String
titleEstimatedValue =
    "Estimated value"


titleValue : String -> String
titleValue coinCode =
    if coinCode == "eth" then
        "Value"

    else
        titleEstimatedValue


filter : Graph.Config -> Table.Filter Api.Data.NeighborAddress
filter { showZeroTransactions } =
    { search =
        \term a ->
            String.contains term a.address.address
                || (Maybe.map (List.any (String.contains term)) a.labels |> Maybe.withDefault True)
    , filter =
        \a ->
            showZeroTransactions || a.address.currency /= "eth" || a.value.value /= 0
    }
