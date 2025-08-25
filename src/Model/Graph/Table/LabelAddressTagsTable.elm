module Model.Graph.Table.LabelAddressTagsTable exposing (filter, titleConfidence)

import Api.Data
import Components.Table as Table


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
