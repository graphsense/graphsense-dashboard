module Effect.Graph exposing (..)

import Browser.Dom
import Effect.Api as Api
import Effect.Search as Search
import IntDict exposing (IntDict)
import Json.Encode
import Model.Address as A
import Model.Entity as E
import Model.Graph.Id as Id exposing (AddressId, EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Graph.Search exposing (Criterion)
import Msg.Graph exposing (Msg(..))
import Plugin.Msg as Plugin
import Route.Graph exposing (Route)
import Set exposing (Set)
import Task


type Effect
    = NavPushRouteEffect Route
    | GetSvgElementEffect
    | GetBrowserElementEffect
    | PluginEffect (Cmd Plugin.Msg)
    | InternalGraphAddedAddressesEffect (Set AddressId)
    | InternalGraphAddedEntitiesEffect (Set EntityId)
    | TagSearchEffect Search.Effect
    | ApiEffect (Api.Effect Msg)
    | CmdEffect (Cmd Msg)


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        -- managed in Effect.elm
        NavPushRouteEffect str ->
            Cmd.none

        GetSvgElementEffect ->
            Browser.Dom.getElement "graph"
                |> Task.attempt BrowserGotSvgElement

        GetBrowserElementEffect ->
            Browser.Dom.getElement "propertyBox"
                |> Task.attempt BrowserGotBrowserElement

        -- managed in Effect.elm
        InternalGraphAddedAddressesEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        InternalGraphAddedEntitiesEffect _ ->
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
