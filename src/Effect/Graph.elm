module Effect.Graph exposing (..)

import Api.Data
import Api.Request.Entities
import Browser.Dom
import Effect.Search as Search
import IntDict exposing (IntDict)
import Json.Encode
import Model.Address as A
import Model.Entity as E
import Model.Graph.Id exposing (AddressId, EntityId)
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
    | GetAddressEffect
        { currency : String
        , address : String
        , toMsg : Api.Data.Address -> Msg
        , suppressErrors : Bool
        }
    | GetEntityEffect
        { currency : String
        , entity : Int
        , toMsg : Api.Data.Entity -> Msg
        }
    | GetBlockEffect
        { currency : String
        , height : Int
        , toMsg : Api.Data.Block -> Msg
        }
    | GetEntityForAddressEffect
        { currency : String
        , address : String
        , toMsg : Api.Data.Entity -> Msg
        , suppressErrors : Bool
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
    | GetBlockTxsEffect
        { currency : String
        , block : Int
        , pagesize : Int
        , nextpage : Maybe String
        , toMsg : List Api.Data.Tx -> Msg
        }
    | GetEntityAddressTagsEffect
        { currency : String
        , entity : Int
        , pagesize : Int
        , nextpage : Maybe String
        , toMsg : Api.Data.AddressTags -> Msg
        }
    | SearchEntityNeighborsEffect
        { currency : String
        , entity : Int
        , isOutgoing : Bool
        , key : Api.Request.Entities.Key
        , value : List String
        , depth : Int
        , breadth : Int
        , maxAddresses : Int
        , toMsg : List Api.Data.SearchResultLevel1 -> Msg
        }
    | GetTxEffect
        { currency : String
        , txHash : String
        , toMsg : Api.Data.Tx -> Msg
        }
    | GetTxUtxoAddressesEffect
        { currency : String
        , txHash : String
        , isOutgoing : Bool
        , toMsg : List Api.Data.TxValue -> Msg
        }
    | ListAddressTagsEffect
        { label : String
        , nextpage : Maybe String
        , pagesize : Maybe Int
        , toMsg : Api.Data.AddressTags -> Msg
        }
    | GetAddresslinkTxsEffect
        { currency : String
        , source : String
        , target : String
        , nextpage : Maybe String
        , pagesize : Int
        , toMsg : Api.Data.Links -> Msg
        }
    | GetEntitylinkTxsEffect
        { currency : String
        , source : Int
        , target : Int
        , nextpage : Maybe String
        , pagesize : Int
        , toMsg : Api.Data.Links -> Msg
        }
    | BulkGetAddressEffect
        { currency : String
        , addresses : List String
        , toMsg : List Api.Data.Address -> Msg
        }
    | BulkGetAddressTagsEffect
        { currency : String
        , addresses : List String
        , toMsg : List Api.Data.AddressTag -> Msg
        }
    | BulkGetEntityEffect
        { currency : String
        , entities : List Int
        , toMsg : List Api.Data.Entity -> Msg
        }
    | BulkGetAddressEntityEffect
        { currency : String
        , addresses : List String
        , toMsg : List Api.Data.Entity -> Msg
        }
    | BulkGetEntityNeighborsEffect
        { currency : String
        , isOutgoing : Bool
        , entities : List Int
        , toMsg : List ( Int, Api.Data.NeighborEntity ) -> Msg
        }
    | PluginEffect (Cmd Plugin.Msg)
    | InternalGraphAddedAddressesEffect (Set AddressId)
    | InternalGraphAddedEntitiesEffect (Set EntityId)
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
        GetBlockEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetEntityForAddressEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetAddressTxsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetAddresslinkTxsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetEntitylinkTxsEffect _ ->
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
        GetBlockTxsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetEntityAddressTagsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        SearchEntityNeighborsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetTxEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        GetTxUtxoAddressesEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        ListAddressTagsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        BulkGetAddressEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        BulkGetAddressTagsEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        BulkGetEntityEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        BulkGetAddressEntityEffect _ ->
            Cmd.none

        -- managed in Effect.elm
        BulkGetEntityNeighborsEffect _ ->
            Cmd.none

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
