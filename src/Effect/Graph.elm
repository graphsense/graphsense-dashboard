module Effect.Graph exposing (..)

import Api.Data
import Browser.Dom
import Effect.Search as Search
import IntDict exposing (IntDict)
import Json.Encode
import Model.Address as A
import Model.Entity as E
import Model.Graph.Id exposing (AddressId)
import Model.Graph.Layer as Layer exposing (Layer)
import Msg.Graph exposing (Msg(..))
import Plugin.Model as Plugin
import Route.Graph exposing (Route)
import Set exposing (Set)
import Task


type Effect
    = NavPushRouteEffect Route
    | GetSvgElementEffect
    | GetBrowserElementEffect
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
        , includeLabels : Bool
        , nextpage : Maybe String
        , toMsg : Api.Data.NeighborEntities -> Msg
        }
    | GetAddressNeighborsEffect
        { currency : String
        , address : String
        , isOutgoing : Bool
        , pagesize : Int
        , includeLabels : Bool
        , nextpage : Maybe String
        , toMsg : Api.Data.NeighborAddresses -> Msg
        }
    | GetAddressTxsEffect
        { currency : String
        , address : String
        , pagesize : Int
        , nextpage : Maybe String
        , toMsg : Api.Data.AddressTxs -> Msg
        }
    | GetEntityAddressesEffect
        { currency : String
        , entity : Int
        , pagesize : Int
        , nextpage : Maybe String
        , toMsg : Api.Data.EntityAddresses -> Msg
        }
    | GetEntityTxsEffect
        { currency : String
        , entity : Int
        , pagesize : Int
        , nextpage : Maybe String
        , toMsg : Api.Data.AddressTxs -> Msg
        }
    | GetAddressTagsEffect
        { currency : String
        , address : String
        , pagesize : Int
        , nextpage : Maybe String
        , toMsg : Api.Data.AddressTags -> Msg
        }
    | GetEntityAddressTagsEffect
        { currency : String
        , entity : Int
        , pagesize : Int
        , nextpage : Maybe String
        , toMsg : Api.Data.AddressTags -> Msg
        }
    | PluginEffect ( String, Cmd Json.Encode.Value )
    | InternalGraphAddedAddressesEffect (Set AddressId)
    | TagSearchEffect Search.Effect
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

        -- managed in Effect.elm
        GetEntityAddressesEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetEntityTxsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetAddressTagsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetEntityAddressTagsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        InternalGraphAddedAddressesEffect _ ->
            Cmd.none

        PluginEffect ( pid, cmd ) ->
            cmd
                |> Cmd.map (PluginMsg pid)

        -- managed in Effect.elm
        TagSearchEffect _ ->
            Cmd.none

        CmdEffect cmd ->
            cmd


getEntityEgonet :
    { currency : String, entity : Int }
    -> (String -> Int -> Bool -> Api.Data.NeighborEntities -> Msg)
    -> IntDict Layer
    -> List Effect
getEntityEgonet { currency, entity } msg layers =
    let
        -- TODO optimize which only_ids to get for which direction
        onlyIds =
            layers
                |> Layer.entities
                |> List.map (.entity >> .entity)

        effect isOut =
            GetEntityNeighborsEffect
                { currency = currency
                , entity = entity
                , isOutgoing = isOut
                , onlyIds = Just onlyIds
                , pagesize = max 1 <| List.length onlyIds
                , nextpage = Nothing
                , includeLabels = False
                , toMsg = msg currency entity isOut
                }
    in
    [ effect True
    , effect False
    ]
