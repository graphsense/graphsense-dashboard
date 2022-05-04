module Search.Model exposing (..)

import Api.Data
import Bounce exposing (Bounce)
import RemoteData exposing (WebData)


type alias Model =
    { loading : Bool
    , found : Maybe Api.Data.SearchResult
    , input : String
    , bounce : Bounce
    }
