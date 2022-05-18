module Model.Graph.Address exposing (..)

import Api.Data
import Config.Graph exposing (expandHandleWidth)
import Dict exposing (Dict)
import Model.Graph.Id exposing (..)
import Model.Graph.Link exposing (Link)


type alias Address =
    { id : AddressId
    , entityId : EntityId
    , address : Api.Data.Address
    , category : Maybe String
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , links : Links
    }


type Links
    = Links (Dict AddressId (Link Address))


getHeight : Address -> Float
getHeight addr =
    Config.Graph.addressHeight


getInnerWidth : Address -> Float
getInnerWidth _ =
    Config.Graph.addressWidth


getWidth : Address -> Float
getWidth a =
    getInnerWidth a + expandHandleWidth * 2


getX : Address -> Float
getX addr =
    addr.x + addr.dx


getY : Address -> Float
getY addr =
    addr.y + addr.dy
