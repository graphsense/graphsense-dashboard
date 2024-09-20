module Model.Store exposing (Model)

import Api.Data
import Dict exposing (Dict)
import RemoteData exposing (WebData)


type alias Model =
    { addresses : Dict ( String, String ) (WebData Api.Data.Address)
    , entities : Dict ( String, Int ) (WebData Api.Data.Entity)
    }
