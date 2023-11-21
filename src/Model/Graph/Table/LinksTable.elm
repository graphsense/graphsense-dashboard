module Model.Graph.Table.LinksTable exposing (..)

import Api.Data
import Config.Graph as Graph
import Model.Graph.Table as Table


titleUrl : String
titleUrl =
    "Url"


filter : Table.Filter String
filter =
    { search = String.contains
    , filter = always True
    }
