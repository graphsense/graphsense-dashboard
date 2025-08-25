module Model.Graph.Table.LinksTable exposing (filter, titleUrl)

import Components.Table as Table


titleUrl : String
titleUrl =
    "Url"


filter : Table.Filter String
filter =
    { search = String.contains
    , filter = always True
    }
