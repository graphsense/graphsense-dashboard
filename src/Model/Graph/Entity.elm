module Model.Graph.Entity exposing (Entity, Links(..), getActorById, getActorByIdApi, getActorsCount, getActorsStr, getBestActor, getBestActorApi, getHeight, getInnerWidth, getWidth, getX, getY)

import Api.Data
import Color exposing (Color)
import Config.Graph
    exposing
        ( addressHeight
        , entityMinHeight
        , entityPaddingTop
        , expandHandleWidth
        )
import Dict exposing (Dict)
import List.Extra
import Maybe.Extra
import Model.Graph.Address exposing (..)
import Model.Graph.Id exposing (..)
import Model.Graph.Link exposing (Link)
import Model.Graph.Tag as Tag
import Plugin.Model exposing (EntityState)


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


getActorsStr : Entity -> Maybe String
getActorsStr ent =
    ent.entity.actors
        |> Maybe.map (List.map .label)
        |> Maybe.map (String.join ",")


getActorById : Entity -> String -> Maybe Api.Data.LabeledItemRef
getActorById ent actorId =
    getActorByIdApi ent.entity actorId


getActorByIdApi : Api.Data.Entity -> String -> Maybe Api.Data.LabeledItemRef
getActorByIdApi ent actorId =
    List.Extra.find (\x -> x.id == actorId) (ent.actors |> Maybe.withDefault [])


getBestActor : Entity -> Maybe Api.Data.LabeledItemRef
getBestActor ent =
    getBestActorApi ent.entity


getBestActorApi : Api.Data.Entity -> Maybe Api.Data.LabeledItemRef
getBestActorApi ent =
    ent.bestAddressTag
        |> Maybe.map (.actor >> Maybe.map (getActorByIdApi ent))
        |> Maybe.Extra.join
        |> Maybe.Extra.join


getActorsCount : Entity -> Int
getActorsCount ent =
    ent.entity.actors
        |> Maybe.map List.length
        |> Maybe.withDefault 0
