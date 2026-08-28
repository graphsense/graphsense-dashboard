module Route exposing
    ( Config
    , Route(..)
    , parse
    , pathfinderRoute
    , pluginRoute
    , settingsRoute
    , statsRoute
    , toUrl
    )

import Plugin.Model
import Plugin.Route as Plugin
import Route.Pathfinder as Pathfinder
import Url exposing (..)
import Url.Builder exposing (..)
import Util.Url.Parser as P exposing (..)


type alias Config =
    { pathfinder : Pathfinder.Config
    }


type Route
    = Pathfinder Pathfinder.Route
    | Home
    | Stats
    | Settings
    | RetiredGraph
    | Plugin ( Plugin.Model.PluginType, String )


pathfinderSegment : String
pathfinderSegment =
    "pathfinder"


graphSegment : String
graphSegment =
    "graph"


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
        [ map Pathfinder (s pathfinderSegment |> slash (Pathfinder.parser c.pathfinder))
        , map Stats (s statsSegment)
        , map Settings (s settingsSegment)

        -- any /graph/* url of the removed legacy graph tool lands on the
        -- "Pathfinder 1.0 retired" page
        , map (\_ -> RetiredGraph) (s graphSegment |> slash (remainder Just))
        , map RetiredGraph (s graphSegment)
        , map Home top
        , map Plugin (remainder Plugin.parseUrl)
        ]


statsRoute : Route
statsRoute =
    Stats


settingsRoute : Route
settingsRoute =
    Settings


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
        Pathfinder p ->
            absolute [ pathfinderSegment ] [] ++ Pathfinder.toUrl p

        Stats ->
            absolute [ statsSegment ] []

        Settings ->
            absolute [ settingsSegment ] []

        RetiredGraph ->
            absolute [ graphSegment ] []

        Home ->
            absolute [] []

        Plugin ( pid, s ) ->
            absolute
                [ Plugin.Model.pluginTypeToNamespace pid
                , if s |> String.startsWith "/" then
                    String.dropLeft 1 s

                  else
                    s
                ]
                []
