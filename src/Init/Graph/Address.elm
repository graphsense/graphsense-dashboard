module Init.Graph.Address exposing (init)

import Api.Data
import Config.Graph exposing (addressHeight, addressWidth, expandHandleWidth, labelHeight, padding)
import Config.Update exposing (Config)
import Init.Graph.Id as Id exposing (..)
import Model.Graph.Address exposing (..)
import Model.Graph.Entity exposing (..)
import Model.Graph.Id as Id exposing (..)


init : Entity -> Api.Data.Address -> Address
init entity address =
    { id =
        Id.initAddressId
            { layer = Id.layer entity.id
            , currency = address.currency
            , id = address.address
            }
    , address = address
    , category =
        address.tags
            |> Maybe.andThen (List.head >> Maybe.andThen .category)
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
            + (toFloat (List.length entity.addresses) * addressHeight)
    }
