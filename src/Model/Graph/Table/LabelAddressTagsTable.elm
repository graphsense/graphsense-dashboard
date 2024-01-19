module Model.Graph.Table.LabelAddressTagsTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table


titleConfidence : String
titleConfidence =
    "Confidence"


filter : Table.Filter Api.Data.AddressTag
filter =
    { search =
        \term a ->
            String.contains term a.address
                || String.contains term a.label
    , filter = always True
    }
