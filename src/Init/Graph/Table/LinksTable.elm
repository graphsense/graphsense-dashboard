module Init.Graph.Table.LinksTable exposing (init)

import Init.Graph.Table
import Model.Graph.Table exposing (Table)
import Model.Graph.Table.LinksTable exposing (titleUrl)


init : Table String
init =
    Init.Graph.Table.initSorted True titleUrl
