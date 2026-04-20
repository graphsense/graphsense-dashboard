module Effect.Pathfinder exposing (Effect(..), effectToTracker, perform)

import Components.Tooltip as Tooltip
import Components.TransactionFilter as TransactionFilter
import Effect.Api as Api
import Effect.Search as Search
import Model.Notification exposing (Notification)
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
    | BatchEffect (List Effect)
    | CmdEffect (Cmd Msg)
    | SearchEffect Search.Effect
    | ErrorEffect Error
    | RepositionTooltipEffect
    | PostponeUpdateByRouteEffect Route
    | ShowNotificationEffect Notification
    | InternalEffect Msg
    | TransactionFilterEffect TransactionFilter.Effect
    | TooltipEffect Tooltip.Effect


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        -- managed in Effect.elm
        NavPushRouteEffect _ ->
            Cmd.none

        ShowNotificationEffect _ ->
            Cmd.none

        PluginEffect cmd ->
            cmd
                |> Cmd.map PluginMsg

        CmdEffect cmd ->
            cmd

        BatchEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        ApiEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        SearchEffect _ ->
            Cmd.none

        ErrorEffect _ ->
            Cmd.none

        RepositionTooltipEffect ->
            Task.perform (always RepositionTooltip) (Task.succeed ())

        PostponeUpdateByRouteEffect route ->
            Process.sleep 10
                |> Task.perform (\_ -> RuntimePostponedUpdateByRoute route)

        -- managed in Effect.elm
        InternalEffect _ ->
            Cmd.none

        TransactionFilterEffect e ->
            TransactionFilter.perform e
                |> Cmd.map TransactionFilterMsg

        TooltipEffect e ->
            Tooltip.perform e
                |> Cmd.map TooltipMsg


effectToTracker : Effect -> Maybe String
effectToTracker eff =
    case eff of
        ApiEffect e ->
            Api.effectToTracker e

        _ ->
            Nothing
