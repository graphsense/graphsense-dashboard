module Init.Graph.Table.LinksTable exposing (init)

import Components.Table as Table exposing (Table)
import Model.Graph.Table.LinksTable exposing (titleUrl)


init : Table String
init =
    Table.initSorted True titleUrl
