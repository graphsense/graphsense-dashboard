module Route exposing
    ( Config
    , Route(..)
    , parse
    , pathfinderRoute
    , pluginRoute
    , settingsRoute
    , statsRoute
    , toUrl
    , unavailableNetwork
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


{-| The network of a Pathfinder URL that fails to `parse` only because its
network is not among `c`'s networks — a link or a recent search from before
the account lost access to it, or before lite networks were switched off.
Answered by parsing again with that network allowed, so it agrees with the
route grammar by construction.
-}
unavailableNetwork : Config -> Url -> Maybe String
unavailableNetwork c url =
    case String.split "/" url.path |> List.filter (not << String.isEmpty) of
        segment :: network :: _ ->
            if segment == pathfinderSegment && not (List.member network c.pathfinder.networks) then
                case parse { pathfinder = { networks = [ network ] } } url of
                    Just (Pathfinder (Pathfinder.Network _ _)) ->
                        Just network

                    Just (Pathfinder (Pathfinder.Path _ _)) ->
                        Just network

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing


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
