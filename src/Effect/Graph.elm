module Effect.Graph exposing (Effect(..), perform)

import Api.Data
import Browser.Dom
import Msg.Graph exposing (Msg(..))
import Task


type Effect
    = GetSvgElementEffect
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


perform : Effect -> Cmd Msg
perform eff =
    case eff of
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
