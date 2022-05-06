module Init.Graph.Entity exposing (init)

import Api.Data
import Init.Graph.Id exposing (..)
import Model.Graph.Entity exposing (..)


init : { x : Float, y : Float, layer : Int } -> Api.Data.Entity -> Entity
init { x, y, layer } entity =
    { id = initEntityId { layer = layer, currency = entity.currency, id = entity.entity }
    , entity = entity
    , addresses = []
    , x = x
    , y = y
    , dx = 0
    , dy = 0
    }
