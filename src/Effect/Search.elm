module Effect.Search exposing (Effect(..), n)

import Api
import Api.Data
import Api.Request.General
import Http
import Msg.Search exposing (Msg)
import Task
import Time
import Util.Http exposing (Headers)


type Effect
    = NoEffect
    | SearchEffect
        { query : String
        , currency : Maybe String
        , limit : Maybe Int
        , toMsg : Api.Data.SearchResult -> Msg
        }
    | CancelEffect
    | BatchEffect (List Effect)
    | BounceEffect Float Msg


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )
