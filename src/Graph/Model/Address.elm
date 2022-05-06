module Graph.Model.Address exposing (..)

import Api.Data
import Graph.Model.Id exposing (..)


type alias Address =
    { id : AddressId
    , address : Api.Data.Address
    , x : Float
    , y : Float
    }
