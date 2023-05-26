module Effect exposing (n, perform)

--import Plugin.Effect

import Api
import Api.Request.General
import Bounce
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Effect.Api
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Search as Search
import Http
import Model exposing (Auth(..), Effect(..), Msg(..))
import Msg.Graph as Graph
import Msg.Search as Search
import Plugin.Effects as Plugin exposing (Plugins)
import Ports
import Process
import Route
import Task


n : m -> ( m, List eff )
n m =
    ( m, [] )


perform : Plugins -> Nav.Key -> Maybe String -> String -> Effect -> Cmd Msg
perform plugins key statusbarToken apiKey effect =
    case Debug.log "effect" effect of
        NavLoadEffect url ->
            Nav.load url

        NavPushUrlEffect url ->
            Nav.pushUrl key url

        GetStatisticsEffect ->
            Api.Request.General.getStatistics
                |> Api.send BrowserGotStatistics

        GetElementEffect { id, msg } ->
            Dom.getElement id
                |> Task.attempt msg

        GetContentsElementEffect ->
            Dom.getElement "contents"
                |> Task.attempt BrowserGotContentsElement

        LocaleEffect eff ->
            Locale.perform eff
                |> Cmd.map LocaleMsg

        LogoutEffect ->
            Http.riskyRequest
                { method = "GET"
                , headers = [ Http.header "Authorization" apiKey ]
                , url = Api.baseUrl ++ "/search?logout"
                , body = Http.emptyBody
                , expect = Http.expectWhatever BrowserGotLoggedOut
                , timeout = Nothing
                , tracker = Nothing
                }

        ApiEffect eff ->
            Effect.Api.perform apiKey (BrowserGotResponseWithHeaders statusbarToken) eff

        GraphEffect eff ->
            case eff of
                Graph.ApiEffect apiEff ->
                    Effect.Api.map GraphMsg apiEff
                        |> Effect.Api.perform apiKey (BrowserGotResponseWithHeaders statusbarToken)

                Graph.NavPushRouteEffect route ->
                    Route.graphRoute route
                        |> Route.toUrl
                        |> Nav.pushUrl key

                Graph.GetBrowserElementEffect ->
                    Graph.perform eff
                        |> Cmd.map GraphMsg

                Graph.InternalGraphAddedAddressesEffect ids ->
                    Task.succeed ids
                        |> Task.perform (Graph.InternalGraphAddedAddresses >> GraphMsg)

                Graph.InternalGraphAddedEntitiesEffect ids ->
                    Task.succeed ids
                        |> Task.perform (Graph.InternalGraphAddedEntities >> GraphMsg)

                Graph.InternalGraphSelectedAddressEffect id ->
                    Task.succeed id
                        |> Task.perform (Graph.InternalGraphSelectedAddress >> GraphMsg)

                Graph.PluginEffect _ ->
                    Graph.perform eff
                        |> Cmd.map GraphMsg

                Graph.TagSearchEffect e ->
                    handleSearchEffect apiKey
                        Nothing
                        (Graph.TagSearchMsg >> GraphMsg)
                        e

                Graph.CmdEffect cmd ->
                    cmd
                        |> Cmd.map GraphMsg

                Graph.DownloadCSVEffect _ ->
                    Graph.perform eff
                        |> Cmd.map GraphMsg

        SearchEffect e ->
            handleSearchEffect apiKey (Just plugins) SearchMsg e

        PortsConsoleEffect msg ->
            Ports.console msg

        PluginEffect cmd ->
            cmd
                |> Cmd.map PluginMsg

        CmdEffect cmd ->
            cmd


handleSearchEffect : String -> Maybe Plugins -> (Search.Msg -> Msg) -> Search.Effect -> Cmd Msg
handleSearchEffect apiKey plugins tag effect =
    case effect of
        Search.SearchEffect { query, currency, limit, toMsg } ->
            (Effect.Api.SearchEffect
                { query = query
                , currency = currency
                , limit = limit
                }
                (toMsg >> tag)
                |> Effect.Api.perform apiKey (BrowserGotResponseWithHeaders Nothing)
            )
                :: (plugins
                        |> Maybe.map (\p -> Plugin.search p query)
                        |> Maybe.withDefault []
                        |> List.map (Cmd.map PluginMsg)
                   )
                |> Cmd.batch

        Search.CancelEffect ->
            Http.cancel "search"
                |> Cmd.map tag

        Search.BounceEffect delay msg ->
            Bounce.delay delay msg
                |> Cmd.map tag

        Search.BlurBounceEffect ->
            Process.sleep 200
                |> Task.perform (\_ -> Search.BouncedBlur)
                |> Cmd.map tag
