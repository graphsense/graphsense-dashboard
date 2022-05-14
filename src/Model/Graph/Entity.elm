module Model.Graph.Entity exposing (..)

import Api.Data
import Config.Graph exposing (addressHeight, addressesCountHeight, entityMinHeight, labelHeight, padding)
import Config.Update exposing (Config)
import Dict exposing (Dict)
import Model.Graph.Address exposing (..)
import Model.Graph.Id exposing (..)
import Model.Graph.Link exposing (Link)


type alias Entity =
    { id : EntityId
    , entity : Api.Data.Entity
    , addresses : Dict AddressId Address
    , category : Maybe String
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , links : Links
    }


type Links
    = Links (Dict EntityId (Link Entity))


getHeight : Entity -> Float
getHeight entity =
    (toFloat (Dict.size entity.addresses) * addressHeight)
        + entityMinHeight
        + (if Dict.size entity.addresses > 0 then
            1

           else
            0
          )
        * padding


getWidth : Entity -> Float
getWidth _ =
    Config.Graph.entityWidth


getY : Entity -> Float
getY entity =
    entity.y + entity.dy


getX : Entity -> Float
getX entity =
    entity.x + entity.dx
