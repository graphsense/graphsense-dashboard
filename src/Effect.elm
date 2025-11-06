module Effect exposing (perform)

--import Plugin.Effect

import Browser.Dom as Dom
import Browser.Navigation as Nav
import Config.UserSettings
import Effect.Api
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Pathfinder as Pathfinder
import Effect.Search as Search
import Http
import Model exposing (Effect(..), Msg(..))
import Model.Notification
import Model.Pathfinder.Tooltip
import Msg.Graph as Graph
import Msg.Pathfinder as Pathfinder
import Msg.Search as Search
import Plugin.Effects as Plugin exposing (Plugins)
import Ports
import Process
import Route
import Task


perform : Plugins -> Nav.Key -> Maybe String -> String -> Effect -> Cmd Msg
perform plugins key statusbarToken apiKey effect =
    case effect of
        NavLoadEffect url ->
            Nav.load url

        NavPushUrlEffect url ->
            Nav.pushUrl key url

        NavBackEffect ->
            Nav.back key 1

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
                , headers = []
                , url = "/?logout"
                , body = Http.emptyBody
                , expect = Http.expectWhatever BrowserGotLoggedOut
                , timeout = Nothing
                , tracker = Nothing
                }

        SetDirtyEffect ->
            Ports.setDirty True

        SetCleanEffect ->
            Ports.setDirty False

        SaveUserSettingsEffect model ->
            Config.UserSettings.encoder model
                |> Ports.saveToLocalStorage

        ApiEffect eff ->
            Effect.Api.perform apiKey (BrowserGotResponseWithHeaders statusbarToken) eff

        PathfinderEffect eff ->
            case eff of
                Pathfinder.ApiEffect apiEff ->
                    Effect.Api.map PathfinderMsg apiEff
                        |> Effect.Api.perform apiKey (BrowserGotResponseWithHeaders statusbarToken)

                Pathfinder.NavPushRouteEffect route ->
                    Route.pathfinderRoute route
                        |> Route.toUrl
                        |> Nav.pushUrl key

                Pathfinder.PluginEffect _ ->
                    Pathfinder.perform eff
                        |> Cmd.map PathfinderMsg

                Pathfinder.CmdEffect cmd ->
                    cmd
                        |> Cmd.map PathfinderMsg

                Pathfinder.SearchEffect e ->
                    handleSearchEffect apiKey
                        Nothing
                        (Pathfinder.SearchMsg >> PathfinderMsg)
                        e

                Pathfinder.ErrorEffect _ ->
                    Cmd.none

                Pathfinder.PostponeUpdateByRouteEffect _ ->
                    Pathfinder.perform eff
                        |> Cmd.map PathfinderMsg

                Pathfinder.OpenTooltipEffect ctx withDelay tttype ->
                    Task.perform (always (OpeningTooltip ctx withDelay (Model.Pathfinder.Tooltip.mapMsgTooltipType tttype PathfinderMsg))) (Task.succeed ())

                Pathfinder.CloseTooltipEffect ctx withDelay ->
                    Task.perform (always (ClosingTooltip ctx withDelay)) (Task.succeed ())

                Pathfinder.RepositionTooltipEffect ->
                    Task.perform (always RepositionTooltip) (Task.succeed ())

                Pathfinder.ShowNotificationEffect n ->
                    Task.perform (always (ShowNotification n)) (Task.succeed ())

                Pathfinder.InternalEffect msg ->
                    Task.succeed (msg |> PathfinderMsg)
                        |> Task.perform identity

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

        SearchEffect msgMap e ->
            handleSearchEffect apiKey (Just plugins) msgMap e

        NotificationEffect e ->
            Model.Notification.perform e
                |> Cmd.map NotificationMsg

        PortsConsoleEffect msg ->
            Ports.console msg

        PluginEffect cmd ->
            cmd
                |> Cmd.map PluginMsg

        CmdEffect cmd ->
            cmd

        PostponeUpdateByUrlEffect url ->
            Process.sleep 50
                |> Task.perform (\_ -> RuntimePostponedUpdateByUrl url)


handleSearchEffect : String -> Maybe Plugins -> (Search.Msg -> Msg) -> Search.Effect -> Cmd Msg
handleSearchEffect apiKey plugins tag effect =
    case effect of
        Search.SearchEffect { query, currency, limit, config, toMsg } ->
            (Effect.Api.SearchEffect
                { query = query
                , currency = currency
                , limit = limit
                , config = config
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

        Search.CmdEffect cmd ->
            Cmd.map tag cmd
