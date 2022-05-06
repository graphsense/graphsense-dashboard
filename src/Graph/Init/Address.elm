module Graph.Init.Address exposing (init)

import Api.Data
import Graph.Init.Id as Id exposing (..)
import Graph.Model.Address exposing (..)
import Graph.Model.Entity exposing (..)
import Graph.Model.Id as Id exposing (..)
import Graph.View.Config exposing (addressHeight, addressWidth, expandHandleWidth, labelHeight, padding)


init : Entity -> Api.Data.Address -> Address
init entity address =
    { id =
        Id.initAddressId
            { layer = Id.layer entity.id
            , currency = address.currency
            , id = address.address
            }
    , address = address
    , x =
        entity.x
            + entity.dx
            + expandHandleWidth
            + padding
    , y =
        entity.y
            + entity.dy
            + 2
            * padding
            + labelHeight
    }
