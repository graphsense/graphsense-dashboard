module Effect.Search exposing (Effect(..))

import Api
import Api.Data
import Api.Request.General
import Http
import Msg.Search exposing (Msg)
import Task
import Time
import Util.Http exposing (Headers)


type Effect
    = SearchEffect
        { query : String
        , currency : Maybe String
        , limit : Maybe Int
        , toMsg : Api.Data.SearchResult -> Msg
        }
    | CancelEffect
    | BounceEffect Float Msg
