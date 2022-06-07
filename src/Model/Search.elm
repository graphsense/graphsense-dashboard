module Model.Search exposing (..)

import Api.Data
import Bounce exposing (Bounce)
import RemoteData exposing (WebData)


type alias Model =
    { loading : Bool
    , found : Maybe Api.Data.SearchResult
    , input : String
    , bounce : Bounce
    }


type ResultLine
    = Address String
    | Tx String
    | Block Int
    | Label String
