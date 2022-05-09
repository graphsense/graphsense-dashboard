module Model.Store exposing (..)

import Api.Data
import Dict exposing (Dict)
import Effect.Store exposing (Effect)
import RemoteData exposing (WebData)


type alias Model =
    { addresses : Dict ( String, String ) (WebData Api.Data.Address)
    , entities : Dict ( String, Int ) (WebData Api.Data.Entity)
    }
