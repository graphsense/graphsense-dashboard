module Route exposing (Route(..), graphRoute, graphSegment, parse, toUrl)

import List.Extra
import Route.Graph as Graph
import Url exposing (..)
import Url.Builder as B exposing (..)
import Url.Parser as P exposing (..)
import Url.Parser.Query as Q


type alias Config =
    { graph : Graph.Config
    }


type Route
    = Graph Graph.Route
    | Stats


graphSegment : String
graphSegment =
    "graph"


statsSegment : String
statsSegment =
    "stats"


parse : Config -> Url -> Maybe Route
parse c =
    P.parse (parser c)


parser : Config -> Parser (Route -> a) a
parser c =
    oneOf
        [ map Graph (s graphSegment </> Graph.parser c.graph)
        , map Stats (s statsSegment)
        ]


graphRoute : Graph.Route -> Route
graphRoute =
    Graph


toUrl : Route -> String
toUrl route =
    case route of
        Graph graph ->
            absolute [ graphSegment ] [] ++ Graph.toUrl graph

        Stats ->
            absolute [ statsSegment ] []
