module Model.Graph.Table.UserAddressTagsTable exposing (filter, titleAbuse, titleAddress, titleCategory, titleCurrency, titleDefinesEntity, titleLabel, titleSource)

import Config.Graph as Graph
import Model.Graph.Table as Table
import Model.Graph.Tag as Tag


titleAddress : String
titleAddress =
    "Address"


titleCurrency : String
titleCurrency =
    "Currency"


titleLabel : String
titleLabel =
    "Label"


titleDefinesEntity : String
titleDefinesEntity =
    "Defines entity"


titleSource : String
titleSource =
    "Source"


titleCategory : String
titleCategory =
    "Category"


titleAbuse : String
titleAbuse =
    "Abuse"


filter : Table.Filter Tag.UserTag
filter =
    { search =
        \term a ->
            String.contains term a.address
                || String.contains term a.label
    , filter = always True
    }
