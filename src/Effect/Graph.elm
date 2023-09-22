module Effect.Graph exposing (..)

import Browser.Dom
import Effect.Api as Api
import Effect.Search as Search
import File.Download
import Model.Graph.Id exposing (AddressId, EntityId)
import Msg.Graph exposing (Msg(..))
import Plugin.Msg as Plugin
import Route.Graph exposing (Route)
import Set exposing (Set)
import Task


type Effect
    = NavPushRouteEffect Route
    | GetBrowserElementEffect
    | PluginEffect (Cmd Plugin.Msg)
    | InternalGraphAddedAddressesEffect (Set AddressId)
    | InternalGraphAddedEntitiesEffect (Set EntityId)
    | InternalGraphSelectedAddressEffect AddressId
    | TagSearchEffect Search.Effect
    | ApiEffect (Api.Effect Msg)
    | CmdEffect (Cmd Msg)
    | DownloadCSVEffect ( String, String )


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        -- managed in Effect.elm
        NavPushRouteEffect _ ->
            Cmd.none

        GetBrowserElementEffect ->
            Browser.Dom.getElement "propertyBox"
                |> Task.attempt BrowserGotBrowserElement

        -- managed in Effect.elm
        InternalGraphAddedAddressesEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        InternalGraphAddedEntitiesEffect _ ->
            Cmd.none

        InternalGraphSelectedAddressEffect _ ->
            Cmd.none

        PluginEffect cmd ->
            cmd
                |> Cmd.map PluginMsg

        -- managed in Effect.elm
        TagSearchEffect _ ->
            Cmd.none

        CmdEffect cmd ->
            cmd

        ApiEffect _ ->
            Cmd.none

        DownloadCSVEffect ( name, data ) ->
            File.Download.string (name ++ ".csv") "text/csv" data
