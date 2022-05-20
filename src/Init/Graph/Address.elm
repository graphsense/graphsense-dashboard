module Init.Graph.Address exposing (init)

import Api.Data
import Config.Graph exposing (addressHeight, addressWidth, expandHandleWidth, labelHeight, padding)
import Config.Update exposing (Config)
import Dict
import Init.Graph.Id as Id exposing (..)
import Json.Encode
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
    , entityId = entity.id
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
            + (toFloat (Dict.size entity.addresses) * addressHeight)
    , dx = 0
    , dy = 0
    , links = Model.Graph.Address.Links Dict.empty
    , plugins = Dict.fromList [ ( "casemgm", Json.Encode.string "X" ) ]
    }
