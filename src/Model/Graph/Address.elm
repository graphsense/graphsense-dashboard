module Model.Graph.Address exposing (..)

--import Plugin.Model as Plugin exposing (PluginStates)

import Api.Data
import Color exposing (Color)
import Config.Graph exposing (expandHandleWidth)
import Dict exposing (Dict)
import Json.Encode
import List.Extra
import Model.Graph.Id exposing (..)
import Model.Graph.Link exposing (Link)
import Model.Graph.Tag as Tag
import Plugin.Model as Plugin


type alias Address =
    { id : AddressId
    , entityId : EntityId
    , address : Api.Data.Address
    , tags : Maybe (List Api.Data.AddressTag)
    , category : Maybe String
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , links : Links
    , userTag : Maybe Tag.UserTag
    , color : Maybe Color
    , plugins : Plugin.AddressState
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


tagsToCategory : Maybe (List Api.Data.AddressTag) -> Maybe String
tagsToCategory =
    bestTag >> Maybe.andThen .category


bestTag : Maybe (List Api.Data.AddressTag) -> Maybe Api.Data.AddressTag
bestTag =
    Maybe.andThen
        (List.sortBy (.confidenceLevel >> Maybe.withDefault 0)
            >> List.Extra.last
        )
