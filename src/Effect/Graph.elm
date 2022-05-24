module Effect.Graph exposing (Effect(..), perform)

import Api.Data
import Browser.Dom
import Json.Encode
import Msg.Graph exposing (Msg(..))
import Plugin.Model as Plugin
import Route.Graph exposing (Route)
import Task


type Effect
    = NavPushRouteEffect Route
    | GetSvgElementEffect
    | GetAddressEffect
        { currency : String
        , address : String
        , toMsg : Api.Data.Address -> Msg
        }
    | GetEntityEffect
        { currency : String
        , entity : Int
        , toMsg : Api.Data.Entity -> Msg
        }
    | GetEntityForAddressEffect
        { currency : String
        , address : String
        , toMsg : Api.Data.Entity -> Msg
        }
    | GetEntityNeighborsEffect
        { currency : String
        , entity : Int
        , isOutgoing : Bool
        , pagesize : Int
        , onlyIds : Maybe (List Int)
        , toMsg : Api.Data.NeighborEntities -> Msg
        }
    | GetAddressNeighborsEffect
        { currency : String
        , address : String
        , isOutgoing : Bool
        , pagesize : Int
        , toMsg : Api.Data.NeighborAddresses -> Msg
        }
    | GetAddressTxsEffect
        { currency : String
        , address : String
        , pagesize : Int
        , nextpage : Maybe String
        , toMsg : Api.Data.AddressTxs -> Msg
        }
    | PluginEffect ( String, Cmd Json.Encode.Value )


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        -- managed in Effect.elm
        NavPushRouteEffect str ->
            Cmd.none

        GetSvgElementEffect ->
            Browser.Dom.getElement "graph"
                |> Task.attempt BrowserGotSvgElement

        -- managed in Effect.elm
        GetEntityNeighborsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetAddressNeighborsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetAddressEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetEntityEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetEntityForAddressEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetAddressTxsEffect _ ->
            Cmd.none

        PluginEffect ( pid, cmd ) ->
            cmd
                |> Cmd.map (PluginMsg pid)
