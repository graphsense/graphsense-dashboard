module Model.Graph.Entity exposing (..)

import Api.Data
import Config.Graph exposing (addressHeight, labelHeight, noAddressesLabelHeight, padding)
import Model.Graph.Address exposing (..)
import Model.Graph.Id exposing (..)


type alias Entity =
    { id : EntityId
    , entity : Api.Data.Entity
    , addresses : List Address
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }


calcHeight : Entity -> Float
calcHeight entity =
    (toFloat (List.length entity.addresses) * addressHeight)
        + (2 * padding)
        + labelHeight
        + noAddressesLabelHeight
        + (if List.length entity.addresses > 0 then
            2

           else
            1
          )
        * padding
