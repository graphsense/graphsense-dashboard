module Model.Graph.Table.AddressTagsTable exposing (filter)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table


filter : Table.Filter Api.Data.AddressTag
filter =
    { search =
        \term a ->
            String.contains term a.address
                || String.contains term a.label
    , filter = always True
    }
