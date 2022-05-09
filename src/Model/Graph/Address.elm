module Model.Graph.Address exposing (..)

import Api.Data
import Config.Graph
import Model.Graph.Id exposing (..)


type alias Address =
    { id : AddressId
    , address : Api.Data.Address
    , category : Maybe String
    , x : Float
    , y : Float
    }


getHeight : Address -> Float
getHeight addr =
    Config.Graph.addressHeight


getWidth : Address -> Float
getWidth _ =
    Config.Graph.addressWidth
