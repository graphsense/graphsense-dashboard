module Model.Graph.Adding exposing (..)

import Api.Data
import Dict exposing (Dict)
import RemoteData exposing (WebData)
import Set exposing (Set)


type alias Model =
    { addresses : Dict ( String, String ) (WebData Api.Data.Address)
    , entities : Set ( String, Int )
    , labels : Set String
    }
