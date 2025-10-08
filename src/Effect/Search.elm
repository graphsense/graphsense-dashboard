module Effect.Search exposing (Effect(..))

import Api.Data
import Msg.Search exposing (Msg)


type Effect
    = SearchEffect
        { query : String
        , currency : Maybe String
        , limit : Maybe Int
        , includeSubTxIdentifiers : Maybe Bool
        , toMsg : Api.Data.SearchResult -> Msg
        }
    | CancelEffect
    | CmdEffect (Cmd Msg)
