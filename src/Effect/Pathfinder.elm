module Effect.Pathfinder exposing (Effect(..), perform)

import Effect.Api as Api
import Effect.Search as Search
import Model.Pathfinder.Error exposing (Error)
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Msg as Plugin
import Process
import Route.Pathfinder exposing (Route)
import Task


type Effect
    = NavPushRouteEffect Route
    | PluginEffect (Cmd Plugin.Msg)
    | ApiEffect (Api.Effect Msg)
    | CmdEffect (Cmd Msg)
    | SearchEffect Search.Effect
    | ErrorEffect Error
    | PostponeUpdateByRouteEffect Route


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        -- managed in Effect.elm
        NavPushRouteEffect _ ->
            Cmd.none

        PluginEffect cmd ->
            cmd
                |> Cmd.map PluginMsg

        CmdEffect cmd ->
            cmd

        -- managed in Effect.elm
        ApiEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        SearchEffect _ ->
            Cmd.none

        ErrorEffect _ ->
            Cmd.none

        PostponeUpdateByRouteEffect route ->
            Process.sleep 10
                |> Task.perform (\_ -> RuntimePostponedUpdateByRoute route)
