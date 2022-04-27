module Search.Model exposing (..)

import Api.Data
import Bounce exposing (Bounce)
import RemoteData exposing (WebData)


type alias Model =
    { result : WebData Api.Data.SearchResult
    , found : Maybe Api.Data.SearchResult
    , input : String
    , bounce : Bounce
    }
