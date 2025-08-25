module Model.Graph.Table.EntityNeighborsTable exposing (filter, titleEntityBalance, titleEntityReceived, titleNoAddresses)

import Api.Data
import Components.Table as Table
import Config.Graph as Graph
import Util.Graph as Graph


titleEntityBalance : String
titleEntityBalance =
    "Entity balance"


titleEntityReceived : String
titleEntityReceived =
    "Entity received"


titleNoAddresses : String
titleNoAddresses =
    "No. addresses"


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
