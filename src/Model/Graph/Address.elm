module Model.Graph.Address exposing (..)

import Api.Data
import Model.Graph.Id exposing (..)


type alias Address =
    { id : AddressId
    , address : Api.Data.Address
    , x : Float
    , y : Float
    }
