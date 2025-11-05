module Effect.Search exposing (Effect(..))

import Api.Data
import Effect.Api as Api
import Msg.Search exposing (Msg)


type Effect
    = SearchEffect
        { query : String
        , currency : Maybe String
        , limit : Maybe Int
        , config : Api.SearchRequestConfig
        , toMsg : Api.Data.SearchResult -> Msg
        }
    | CancelEffect
    | CmdEffect (Cmd Msg)
