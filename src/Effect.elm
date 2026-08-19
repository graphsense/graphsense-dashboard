module Effect exposing (perform)

--import Plugin.Effect

import Browser.Dom as Dom
import Browser.Navigation as Nav
import Config
import Config.UserSettings
import Effect.Api
import Effect.Locale as Locale
import Effect.Pathfinder as Pathfinder
import Effect.Search as Search
import Http
import Model exposing (Effect(..), Model, Msg(..))
import Model.Notification
import Msg.Pathfinder as Pathfinder
import Msg.Search as Search
import Plugin.Effects as Plugin
import Ports
import Process
import Route
import Task


perform : Model Nav.Key -> Maybe String -> String -> Effect -> Cmd Msg
perform model statusbarToken apiKey effect =
    case effect of
        NavLoadEffect url ->
            Nav.load url

        NavPushUrlEffect url ->
            Nav.pushUrl model.key url

        NavBackEffect steps ->
            Nav.back model.key steps

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
            Nav.load Config.logoutUrl

        SaveUserSettingsEffect m ->
            Config.UserSettings.encoder m
                |> Ports.saveToLocalStorage

        ApiEffect eff ->
            Effect.Api.perform apiKey
                (BrowserGotResponseWithHeaders statusbarToken)
                BrowserCancelledRequest
                eff

        PathfinderEffect eff ->
            case eff of
                Pathfinder.ApiEffect apiEff ->
                    Effect.Api.map PathfinderMsg apiEff
                        |> Effect.Api.perform apiKey
                            (BrowserGotResponseWithHeaders statusbarToken)
                            BrowserCancelledRequest

                Pathfinder.NavPushRouteEffect route ->
                    Route.pathfinderRoute route
                        |> Route.toUrl
                        |> Nav.pushUrl model.key

                Pathfinder.CmdEffect cmd ->
                    cmd
                        |> Cmd.map PathfinderMsg

                Pathfinder.BatchEffect effs ->
                    effs
                        |> List.map (PathfinderEffect >> perform model statusbarToken apiKey)
                        |> Cmd.batch

                Pathfinder.SearchEffect e ->
                    handleSearchEffect apiKey
                        (Pathfinder.SearchMsg >> PathfinderMsg)
                        e

                Pathfinder.ErrorEffect _ ->
                    Cmd.none

                Pathfinder.ShowNotificationEffect n ->
                    Task.perform (always (ShowNotification n)) (Task.succeed ())

                Pathfinder.InternalEffect msg ->
                    Task.succeed (msg |> PathfinderMsg)
                        |> Task.perform identity

                _ ->
                    Pathfinder.perform eff
                        |> Cmd.map PathfinderMsg

        SearchEffect msgMap e ->
            handleSearchEffect apiKey msgMap e

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


handleSearchEffect : String -> (Search.Msg -> Msg) -> Search.Effect -> Cmd Msg
handleSearchEffect apiKey tag effect =
    case effect of
        Search.SearchEffect { query, currency, limit, config, toMsg } ->
            (Effect.Api.SearchEffect
                { query = query
                , currency = currency
                , limit = limit
                , config = config
                }
                (toMsg >> tag)
                |> Effect.Api.perform apiKey
                    (BrowserGotResponseWithHeaders Nothing)
                    BrowserCancelledRequest
            )
                :: (Plugin.search query
                        |> List.map (Cmd.map PluginMsg)
                   )
                |> Cmd.batch

        Search.CancelEffect ->
            Http.cancel "search"
                |> Cmd.map tag

        Search.CmdEffect cmd ->
            Cmd.map tag cmd
