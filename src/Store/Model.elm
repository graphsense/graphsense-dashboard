module Store.Model exposing (..)

import Api.Data
import Dict exposing (Dict)
import Store.Effect exposing (Effect)


type alias Model =
    { addresses : Dict ( String, String ) Api.Data.Address
    , entities : Dict ( String, Int ) Api.Data.Entity
    }
