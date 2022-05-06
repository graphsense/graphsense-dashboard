module Model.Store exposing (..)

import Api.Data
import Dict exposing (Dict)
import Effect.Store exposing (Effect)


type alias Model =
    { addresses : Dict ( String, String ) Api.Data.Address
    , entities : Dict ( String, Int ) Api.Data.Entity
    }
