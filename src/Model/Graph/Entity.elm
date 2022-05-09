module Model.Graph.Entity exposing (..)

import Api.Data
import Config.Graph exposing (addressHeight, addressesCountHeight, labelHeight, padding)
import Config.Update exposing (Config)
import Model.Graph.Address exposing (..)
import Model.Graph.Id exposing (..)


type alias Entity =
    { id : EntityId
    , entity : Api.Data.Entity
    , addresses : List Address
    , category : Maybe String
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    }


getHeight : Entity -> Float
getHeight entity =
    (toFloat (List.length entity.addresses) * addressHeight)
        + (2 * padding)
        + labelHeight
        + addressesCountHeight
        + (if List.length entity.addresses > 0 then
            2

           else
            1
          )
        * padding


getWidth : Entity -> Float
getWidth _ =
    Config.Graph.entityWidth
