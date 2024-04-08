module Effect.Pathfinder exposing (..)

import Effect.Api as Api
import Msg.Pathfinder exposing (Msg(..))
import Plugin.Msg as Plugin
import Route.Pathfinder exposing (Route)


type Effect
    = NavPushRouteEffect Route
    | PluginEffect (Cmd Plugin.Msg)
    | ApiEffect (Api.Effect Msg)
    | CmdEffect (Cmd Msg)


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
