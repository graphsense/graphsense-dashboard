module Model.Graph.Entity exposing (..)

import Api.Data
import Color exposing (Color)
import Config.Graph
    exposing
        ( addressHeight
        , addressesCountHeight
        , entityMinHeight
        , entityPaddingTop
        , expandHandleWidth
        , labelHeight
        , padding
        )
import Config.Update exposing (Config)
import Dict exposing (Dict)
import Model.Graph.Address exposing (..)
import Model.Graph.Id exposing (..)
import Model.Graph.Link exposing (Link)
import Model.Graph.Tag as Tag
import Plugin.Model as Plugin exposing (EntityState)


type alias Entity =
    { id : EntityId
    , entity : Api.Data.Entity
    , addresses : Dict AddressId Address
    , category : Maybe String
    , addressTags : List Api.Data.AddressTag
    , x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , links : Links
    , shadowLinks : Links
    , color : Maybe Color
    , userTag : Maybe Tag.UserTag
    , selected : Bool
    , plugins : EntityState
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
        * entityPaddingTop


getInnerWidth : Entity -> Float
getInnerWidth _ =
    Config.Graph.entityWidth


getWidth : Entity -> Float
getWidth e =
    getInnerWidth e + expandHandleWidth * 2


getY : Entity -> Float
getY entity =
    entity.y + entity.dy


getX : Entity -> Float
getX entity =
    entity.x + entity.dx