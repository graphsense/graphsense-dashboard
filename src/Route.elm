module Route exposing
    ( Config
    , Route(..)
    , graphRoute
    , homeRoute
    , parse
    , pathfinderRoute
    , pluginRoute
    , settingsRoute
    , statsRoute
    , toUrl
    )

import Plugin.Model
import Plugin.Route as Plugin
import Route.Graph as Graph
import Route.Pathfinder as Pathfinder
import Url exposing (..)
import Url.Builder exposing (..)
import Util.Url.Parser as P exposing (..)


type alias Config =
    { graph : Graph.Config
    , pathfinder : Pathfinder.Config
    }


type Route
    = Graph Graph.Route
    | Pathfinder Pathfinder.Route
    | Home
    | Stats
    | Settings
    | Plugin ( Plugin.Model.PluginType, String )


graphSegment : String
graphSegment =
    "graph"


pathfinderSegment : String
pathfinderSegment =
    "pathfinder"


statsSegment : String
statsSegment =
    "stats"


settingsSegment : String
settingsSegment =
    "settings"


parse : Config -> Url -> Maybe Route
parse c =
    P.parse (parser c)


parser : Config -> Parser (Route -> a) a
parser c =
    oneOf
        [ map Graph (s graphSegment |> slash (Graph.parser c.graph))
        , map Pathfinder (s pathfinderSegment |> slash (Pathfinder.parser c.pathfinder))
        , map Stats (s statsSegment)
        , map Settings (s settingsSegment)
        , map Home top
        , map Plugin (remainder Plugin.parseUrl)
        ]


homeRoute : Route
homeRoute =
    Home


statsRoute : Route
statsRoute =
    Stats


settingsRoute : Route
settingsRoute =
    Settings


graphRoute : Graph.Route -> Route
graphRoute =
    Graph


pathfinderRoute : Pathfinder.Route -> Route
pathfinderRoute =
    Pathfinder


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

        Pathfinder p ->
            absolute [ pathfinderSegment ] [] ++ Pathfinder.toUrl p

        Stats ->
            absolute [ statsSegment ] []

        Settings ->
            absolute [ settingsSegment ] []

        Home ->
            absolute [] []

        Plugin ( pid, _ ) ->
            absolute [ Plugin.Model.pluginTypeToNamespace pid ] []
