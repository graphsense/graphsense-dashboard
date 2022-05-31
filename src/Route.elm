module Route exposing (Route(..), graphRoute, graphSegment, parse, pluginRoute, toUrl)

import List.Extra
import Plugin exposing (Plugins)
import Route.Graph as Graph
import Url exposing (..)
import Url.Builder as B exposing (..)
import Util.Url.Parser as P exposing (..)
import Util.Url.Parser.Query as Q


type alias Config =
    { graph : Graph.Config
    }


type Route
    = Graph Graph.Route
    | Stats
    | Plugin ( String, String )


graphSegment : String
graphSegment =
    "graph"


parse : Plugins -> Config -> Url -> Maybe Route
parse plugins c =
    P.parse (parser plugins c)


parser : Plugins -> Config -> Parser (Route -> a) a
parser plugins c =
    oneOf
        [ map Graph (s graphSegment |> slash (Graph.parser plugins c.graph))
        , map Stats top
        ]


graphRoute : Graph.Route -> Route
graphRoute =
    Graph


pluginRoute : ( String, String ) -> Route
pluginRoute =
    Plugin


toUrl : Route -> String
toUrl route =
    case route of
        Graph graph ->
            absolute [ graphSegment ] [] ++ Graph.toUrl graph

        Stats ->
            absolute [] []

        Plugin ( pid, _ ) ->
            absolute [ pid ] []
