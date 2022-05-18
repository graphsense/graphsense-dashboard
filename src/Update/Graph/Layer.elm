module Update.Graph.Layer exposing (Acc, addAddress, addEntity, addNeighbors, moveEntity, releaseEntity, updateLinks)

import Api.Data
import Color exposing (Color)
import Config.Graph as Graph exposing (padding, txMaxWidth)
import Config.Update as Update
import Dict exposing (Dict)
import Init.Graph.Entity as Entity
import Init.Graph.Id as Id
import Init.Graph.Layer as Layer
import IntDict exposing (IntDict)
import List.Extra
import Log
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (Entity, Links(..))
import Model.Graph.Id as Id exposing (AddressId, EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Locale as Locale
import Set exposing (Set)
import Tuple exposing (..)
import Update.Graph.Color as Color
import Update.Graph.Entity as Entity


type alias Position =
    { y : Float }


type alias Acc comparable =
    { layers : IntDict Layer
    , new : Set comparable
    , repositioned : Set EntityId
    , colors : Dict String Color
    }


{-| Add an address to every entity node it belongs to
-}
addAddress : Update.Config -> Dict String Color -> Api.Data.Address -> IntDict Layer -> Acc AddressId
addAddress uc colors address layers =
    addAddressHelp uc address { layers = layers, new = Set.empty, repositioned = Set.empty, colors = colors }


{-| Add an entity to the root of the graph
-}
addEntity : Update.Config -> Dict String Color -> Api.Data.Entity -> IntDict Layer -> Acc EntityId
addEntity uc colors entity layers =
    addEntitiesAt uc
        (anchorsToPositions IntDict.empty layers)
        [ entity ]
        { layers = layers
        , new = Set.empty
        , colors = colors
        , repositioned = Set.empty
        }


{-| Add neighbors next to an entity
-}
addNeighbors : Update.Config -> Entity -> Bool -> Dict String Color -> List Api.Data.Entity -> IntDict Layer -> Acc EntityId
addNeighbors uc entity isOutgoing colors neighbors layers =
    addEntitiesAt uc
        (anchorsToPositions (IntDict.singleton (Id.layer entity.id) ( entity, isOutgoing )) layers)
        neighbors
        { layers = layers
        , new = Set.empty
        , colors = colors
        , repositioned = Set.empty
        }


anchorsToPositions : IntDict ( Entity, Bool ) -> IntDict Layer -> IntDict Position
anchorsToPositions anchors layers =
    if IntDict.isEmpty anchors then
        let
            y =
                IntDict.get 0 layers
                    |> Maybe.andThen
                        (\layer ->
                            layer.entities
                                |> Dict.foldl
                                    (\_ e max ->
                                        max
                                            |> Maybe.map
                                                (\mx ->
                                                    if mx.y < e.y then
                                                        e

                                                    else
                                                        mx
                                                )
                                            |> Maybe.withDefault e
                                            |> Just
                                    )
                                    Nothing
                                |> Maybe.map (\e -> e.y + Entity.getHeight e + padding)
                        )
                    |> Maybe.withDefault 0
        in
        IntDict.singleton 0 { y = y }

    else
        anchors
            |> IntDict.foldl
                (\i ( entity, isOutgoing ) positions ->
                    let
                        id =
                            Id.layer entity.id
                                + (if isOutgoing then
                                    1

                                   else
                                    -1
                                  )
                    in
                    IntDict.insert id
                        { y =
                            entity.y
                                + entity.dy
                                + (Entity.getHeight entity / 2)
                                - Graph.entityMinHeight
                                / 2
                        }
                        positions
                )
                IntDict.empty


addEntitiesAt : Update.Config -> IntDict Position -> List Api.Data.Entity -> Acc EntityId -> Acc EntityId
addEntitiesAt uc positions entities acc =
    IntDict.foldl
        (\layerId position acc_ ->
            IntDict.get layerId acc_.layers
                |> Maybe.withDefault (Layer.init layerId)
                |> (\layer ->
                        let
                            accToLayer =
                                List.foldl
                                    (\entity ->
                                        addEntityHere uc position entity
                                    )
                                    { layer = layer
                                    , colors = acc_.colors
                                    , new = acc_.new
                                    , repositioned = acc_.repositioned
                                    }
                                    entities
                        in
                        { layers = IntDict.insert layerId accToLayer.layer acc_.layers
                        , new = accToLayer.new
                        , colors = accToLayer.colors
                        , repositioned = accToLayer.repositioned
                        }
                   )
        )
        acc
        positions


type alias AccEntity =
    { layer : Layer
    , colors : Dict String Color
    , new : Set EntityId
    , repositioned : Set EntityId
    }


addEntityHere : Update.Config -> Position -> Api.Data.Entity -> AccEntity -> AccEntity
addEntityHere uc position entity { layer, colors, new, repositioned } =
    let
        entityId =
            Id.initEntityId { currency = entity.currency, id = entity.entity, layer = layer.id }

        ( ( newEntities, newRepositioned ), newEntity ) =
            case Dict.get entityId layer.entities of
                Just ent ->
                    ( ( layer.entities, Set.empty ), ent )

                Nothing ->
                    let
                        newEnt =
                            Entity.init
                                { x = layer.x
                                , y = position.y
                                , layer = layer.id
                                }
                                entity
                    in
                    ( Entity.repositionAround newEnt layer.entities
                    , newEnt
                    )
    in
    { layer =
        { layer
            | entities = newEntities
        }
    , new = Set.insert newEntity.id new
    , repositioned = Set.union newRepositioned repositioned
    , colors = Color.update uc colors newEntity.category
    }


moveEntity : EntityId -> Coords -> IntDict Layer -> IntDict Layer
moveEntity id vector layers =
    updateEntity id (Entity.move vector) layers
        |> first


releaseEntity : EntityId -> IntDict Layer -> IntDict Layer
releaseEntity id layers =
    updateEntity id Entity.release layers
        |> first


updateEntity : EntityId -> (Entity -> ( Entity, a )) -> IntDict Layer -> ( IntDict Layer, Maybe a )
updateEntity id update layers =
    layers
        |> IntDict.get (Id.layer id)
        |> Maybe.andThen
            (\layer ->
                Dict.get id layer.entities
                    |> Maybe.map
                        (\entity ->
                            let
                                ( newEntity, a ) =
                                    update entity
                            in
                            ( IntDict.insert layer.id { layer | entities = Dict.insert id newEntity layer.entities } layers
                            , Just a
                            )
                        )
            )
        |> Maybe.withDefault ( layers, Nothing )


addAddressHelp : Update.Config -> Api.Data.Address -> Acc AddressId -> Acc AddressId
addAddressHelp uc address acc =
    acc.layers
        |> IntDict.foldl
            (\layerId layer acc_ ->
                let
                    accEntity =
                        Entity.addAddress uc
                            layerId
                            address
                            { entities = layer.entities
                            , new = acc_.new
                            , colors = acc_.colors
                            , repositioned = acc_.repositioned
                            }
                in
                { layers =
                    IntDict.insert layer.id { layer | entities = accEntity.entities } acc_.layers
                , new = accEntity.new
                , colors = accEntity.colors
                , repositioned = accEntity.repositioned
                }
            )
            acc


updateLinks : { currency : String, entity : Int } -> List ( Api.Data.NeighborEntity, Entity ) -> IntDict Layer -> IntDict Layer
updateLinks { currency, entity } neighbors layers =
    IntDict.foldl
        (\_ layer ( neighbors_, layers_ ) ->
            let
                neighbors__ =
                    neighbors_
                        |> List.filter (second >> .id >> Id.layer >> (<) layer.id)

                relevant =
                    neighbors__
                        |> List.filter (second >> .id >> Id.layer >> (==) (layer.id + 1))
            in
            ( neighbors__
            , case Dict.get (Id.initEntityId { currency = currency, id = entity, layer = layer.id }) layer.entities of
                Nothing ->
                    layers_

                Just found ->
                    layers_
                        |> IntDict.insert layer.id
                            { layer
                                | entities =
                                    Dict.insert found.id
                                        { found
                                            | links =
                                                insertLinks relevant found.links
                                        }
                                        layer.entities
                            }
            )
        )
        ( neighbors, layers )
        layers
        |> second


insertLinks : List ( Api.Data.NeighborEntity, Entity ) -> Entity.Links -> Entity.Links
insertLinks neighbors (Links links) =
    neighbors
        |> List.foldl
            (\( link, entity ) li ->
                Dict.insert entity.id
                    { value = link.value
                    , noTxs = link.noTxs
                    , labels = link.labels
                    , node = entity
                    }
                    li
            )
            links
        |> Entity.Links


syncLinks : Set EntityId -> IntDict Layer -> IntDict Layer
syncLinks updatedIds layers =
    let
        updated =
            Set.toList updatedIds
                |> List.filterMap (\e -> Layer.getEntity e layers)
    in
    IntDict.map
        (\_ layer ->
            let
                relevant =
                    updated
                        |> List.filter (.id >> Id.layer >> (<) layer.id)
            in
            layer.entities
                |> Dict.foldl
                    (\_ entity layer_ ->
                        relevant
                            |> List.foldl
                                (\updEnt layer__ ->
                                    case entity.links of
                                        Entity.Links links ->
                                            case Dict.get (Log.log "updEnt.id" updEnt.id) links |> Log.log "updEnt " of
                                                Nothing ->
                                                    layer__

                                                Just link ->
                                                    { layer__
                                                        | entities =
                                                            Dict.insert entity.id
                                                                { entity
                                                                    | links =
                                                                        Dict.insert updEnt.id { link | node = updEnt } links
                                                                            |> Entity.Links
                                                                }
                                                                layer__.entities
                                                    }
                                )
                                layer_
                    )
                    layer
        )
        layers
