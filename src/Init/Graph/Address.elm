module Init.Graph.Address exposing (init, initY)

import Api.Data
import Config.Graph
    exposing
        ( addressHeight
        , addressWidth
        , entityToAddressesPaddingLeft
        , entityToAddressesPaddingTop
        , expandHandleWidth
        , labelHeight
        , padding
        )
import Config.Update exposing (Config)
import Dict
import Init.Graph.Id as Id exposing (..)
import Json.Encode
import Model.Graph.Address exposing (..)
import Model.Graph.Entity exposing (..)
import Model.Graph.Id as Id exposing (..)
import Plugin.Update as Plugin exposing (Plugins)


init : Plugins -> Entity -> Api.Data.Address -> Address
init plugins entity address =
    { id =
        Id.initAddressId
            { layer = Id.layer entity.id
            , currency = address.currency
            , id = address.address
            }
    , entityId = entity.id
    , address = address
    , tags = Nothing
    , category = Nothing
    , x =
        entity.x
            + entity.dx
            + entityToAddressesPaddingLeft
    , y = initY entity
    , dx = 0
    , dy = 0
    , links = Model.Graph.Address.Links Dict.empty
    , shadowLinks = Model.Graph.Address.Links Dict.empty
    , userTag = Nothing
    , color = Nothing
    , selected = False
    , plugins = Plugin.initAddress plugins
    }


initY : Entity -> Float
initY entity =
    entity.y
        + entity.dy
        + entityToAddressesPaddingTop
        + (toFloat (Dict.size entity.addresses) * addressHeight)
