module Init.Graph.Entity exposing (init)

import Api.Data
import Dict
import Init.Graph.Id exposing (..)
import Model.Graph.Entity exposing (..)
import Plugin.Update as Plugin exposing (Plugins)


init : Plugins -> { x : Float, y : Float, layer : Int } -> Api.Data.Entity -> Entity
init plugins { x, y, layer } entity =
    { id = initEntityId { layer = layer, currency = entity.currency, id = entity.entity }
    , entity = entity
    , addresses = Dict.empty
    , category =
        entity.bestAddressTag
            |> Maybe.andThen .category
    , addressTags = []
    , x = x
    , y = y
    , dx = 0
    , dy = 0
    , links = Links Dict.empty
    , shadowLinks = Links Dict.empty
    , color = Nothing
    , userTag = Nothing
    , selected = False
    , plugins = Plugin.initEntity plugins
    }
