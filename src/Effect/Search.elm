module Effect.Search exposing (Effect(..))

import Api.Data
import Msg.Search exposing (Msg)

type Effect
    = SearchEffect
        { query : String
        , currency : Maybe String
        , limit : Maybe Int
        , toMsg : Api.Data.SearchResult -> Msg
        }
    | BlurBounceEffect
    | CancelEffect
    | BounceEffect Float Msg
