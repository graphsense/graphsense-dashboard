module Model.Graph.Table.EntityNeighborsTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table
import Util.Graph as Graph


titleLabels : String
titleLabels =
    "Tags"


titleEntityBalance : String
titleEntityBalance =
    "Entity balance"


titleEntityReceived : String
titleEntityReceived =
    "Entity received"


titleNoAddresses : String
titleNoAddresses =
    "No. addresses"


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


filter : Graph.Config -> Table.Filter Api.Data.NeighborEntity
filter gc =
    { search =
        \term a ->
            String.contains term (String.fromInt a.entity.entity)
                || (Maybe.map (List.any (String.contains term)) a.labels |> Maybe.withDefault True)
    , filter =
        \a ->
            Graph.filterTxValue gc a.entity.currency a.value a.tokenValues
    }
