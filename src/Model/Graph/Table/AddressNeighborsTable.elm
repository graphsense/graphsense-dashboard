module Model.Graph.Table.AddressNeighborsTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table
import Util.Graph as Graph
import Util.Data as Data
import Model.Currency exposing (assetFromBase)
import Model.Currency exposing (AssetIdentifier)


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
    "Estimated total transferred"


titleValue : String -> String
titleValue coinCode =
    if Data.isAccountLike(coinCode) then
        "Total transferred"

    else
        titleEstimatedValue


filter : Graph.Config -> Table.Filter Api.Data.NeighborAddress
filter gc =
    { search =
        \term a ->
            String.contains term a.address.address
                || (Maybe.map (List.any (String.contains term)) a.labels |> Maybe.withDefault True)
    , filter =
        \a ->
            Graph.filterTxValue gc a.address.currency a.value a.tokenValues
    }
