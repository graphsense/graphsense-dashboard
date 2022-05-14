module Effect.Graph exposing (Effect(..), perform)

import Api.Data
import Browser.Dom
import Msg.Graph exposing (Msg(..))
import Task


type Effect
    = GetSvgElementEffect
    | GetEntityNeighborsEffect
        { currency : String
        , entity : Int
        , isOutgoing : Bool
        , pagesize : Int
        , onlyIds : Maybe (List Int)
        , toMsg : Api.Data.NeighborEntities -> Msg
        }


perform : Effect -> Cmd Msg
perform eff =
    case eff of
        GetSvgElementEffect ->
            Browser.Dom.getElement "graph"
                |> Task.attempt BrowserGotSvgElement

        -- managed in Update.elm
        GetEntityNeighborsEffect _ ->
            Cmd.none
