module Route exposing (Route(..), graphRoute, graphSegment, parse, pluginRoute, statsRoute, toUrl)

import List.Extra
import Plugin.Model
import Plugin.Route as Plugin
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
    | Plugin ( Plugin.Model.PluginType, String )


graphSegment : String
graphSegment =
    "graph"


parse : Config -> Url -> Maybe Route
parse c =
    P.parse (parser c)


parser : Config -> Parser (Route -> a) a
parser c =
    oneOf
        [ map Graph (s graphSegment |> slash (Graph.parser c.graph))
        , map Stats top
        , map Plugin (remainder Plugin.parseUrl)
        ]


statsRoute : Route
statsRoute =
    Stats


graphRoute : Graph.Route -> Route
graphRoute =
    Graph


pluginRoute : ( String, String ) -> Route
pluginRoute ( ns, url ) =
    ns
        |> Plugin.Model.namespaceToPluginType
        |> Maybe.map
            (\type_ ->
                ( type_
                , url
                )
                    |> Plugin
            )
        |> Maybe.withDefault Stats


toUrl : Route -> String
toUrl route =
    case route of
        Graph graph ->
            absolute [ graphSegment ] [] ++ Graph.toUrl graph

        Stats ->
            absolute [] []

        Plugin ( pid, _ ) ->
            absolute [ Plugin.Model.pluginTypeToNamespace pid ] []