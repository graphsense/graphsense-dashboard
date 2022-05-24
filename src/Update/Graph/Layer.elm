module Update.Graph.Layer exposing
    ( Acc
    , addAddress
    , addAddressAtEntity
    , addEntity
    , addEntityNeighbors
    , moveEntity
    , releaseEntity
    , syncLinks
    , updateAddress
    , updateAddressLink
    , updateAddresses
    , updateEntityLinks
    )

import Api.Data
import Color exposing (Color)
import Config.Graph as Graph exposing (entityWidth, expandHandleWidth, layerMargin, padding, txMaxWidth)
import Config.Update as Update
import Dict exposing (Dict)
import Init.Graph.Entity as Entity
import Init.Graph.Id as Id
import Init.Graph.Layer as Layer
import IntDict exposing (IntDict)
import List.Extra
import Log
import Maybe.Extra
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id exposing (AddressId, EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Locale as Locale
import Plugin exposing (Plugins)
import Set exposing (Set)
import Tuple exposing (..)
import Update.Graph.Color as Color
import Update.Graph.Entity as Entity


type alias Position =
    { x : Float
    , y : Float
    }


type alias Acc comparable =
    { layers : IntDict Layer
    , new : Set comparable
    , repositioned : Set EntityId
    , colors : Dict String Color
    }


{-| Add an address to every entity node it belongs to
-}
addAddress : Plugins -> Update.Config -> Dict String Color -> Api.Data.Address -> IntDict Layer -> Acc AddressId
addAddress plugins uc colors address layers =
    addAddressHelp plugins uc address { layers = layers, new = Set.empty, repositioned = Set.empty, colors = colors }


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


addAddressAtEntity : Plugins -> Update.Config -> Dict String Color -> EntityId -> Api.Data.Address -> IntDict Layer -> Acc AddressId
addAddressAtEntity plugins uc colors entityId address layers =
    IntDict.get (Id.layer entityId) layers
        |> Maybe.map
            (\layer ->
                let
                    accEntity =
                        Entity.addAddress plugins
                            uc
                            layer.id
                            address
                            { entities = layer.entities
                            , new = Set.empty
                            , colors = colors
                            , repositioned = Set.empty
                            }
                in
                { layers =
                    IntDict.insert layer.id { layer | entities = accEntity.entities } layers
                , new = accEntity.new
                , colors = accEntity.colors
                , repositioned = accEntity.repositioned
                }
            )
        |> Maybe.withDefault
            { layers = layers
            , new = Set.empty
            , repositioned = Set.empty
            , colors = colors
            }


{-| Add neighbors next to an entity
-}
addEntityNeighbors : Update.Config -> Entity -> Bool -> Dict String Color -> List Api.Data.Entity -> IntDict Layer -> Acc EntityId
addEntityNeighbors uc entity isOutgoing colors neighbors layers =
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
        IntDict.singleton 0 { x = 0, y = y }

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

                        x =
                            IntDict.get id layers
                                |> Maybe.map Layer.getX
                                |> Maybe.Extra.withDefaultLazy
                                    (\_ ->
                                        IntDict.get (Id.layer entity.id) layers
                                            |> Maybe.map
                                                (\l ->
                                                    Debug.log "getX" (Layer.getX l)
                                                        + (if isOutgoing then
                                                            entityWidth + layerMargin

                                                           else
                                                            -entityWidth - layerMargin
                                                          )
                                                )
                                            |> Maybe.withDefault 0
                                    )
                    in
                    IntDict.insert id
                        { x = x
                        , y =
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
                |> Maybe.withDefault (Layer.init position.x layerId)
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
                        leftBound =
                            Layer.getRightBound layer

                        rightBound =
                            Layer.getLeftBound layer

                        newEnt =
                            Entity.init
                                { x = position.x
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
    IntDict.get (Id.layer id) layers
        |> Maybe.map
            (\layer ->
                let
                    leftBound =
                        IntDict.get (Id.layer id - 1) layers
                            |> Maybe.map Layer.getRightBound

                    rightBound =
                        IntDict.get (Id.layer id + 1) layers
                            |> Maybe.map Layer.getLeftBound

                    boundingBox =
                        { left = leftBound
                        , right = rightBound
                        , upper = Nothing
                        , lower = Nothing
                        }
                in
                updateEntity id (Entity.move boundingBox vector) layers
                    |> first
            )
        |> Maybe.withDefault layers


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


addAddressHelp : Plugins -> Update.Config -> Api.Data.Address -> Acc AddressId -> Acc AddressId
addAddressHelp plugins uc address acc =
    acc.layers
        |> IntDict.foldl
            (\layerId layer acc_ ->
                let
                    accEntity =
                        Entity.addAddress plugins
                            uc
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


updateAddressLink : { currency : String, address : String } -> ( Api.Data.NeighborAddress, Address ) -> IntDict Layer -> IntDict Layer
updateAddressLink { currency, address } ( neighbor, target ) layers =
    layers
        |> IntDict.foldl
            (\_ layer layers_ ->
                if layer.id >= Id.layer target.id then
                    layers_

                else
                    let
                        addressId =
                            Id.initAddressId { currency = currency, id = address, layer = layer.id }

                        ( entities, updated ) =
                            updateAddressLinkForEntities addressId ( neighbor, target ) layer.entities
                    in
                    if updated then
                        IntDict.insert layer.id
                            { layer | entities = entities }
                            layers_

                    else
                        layers_
            )
            layers


updateAddressLinkForEntities : AddressId -> ( Api.Data.NeighborAddress, Address ) -> Dict EntityId Entity -> ( Dict EntityId Entity, Bool )
updateAddressLinkForEntities id ( neighbor, address ) entities =
    entities
        |> Dict.foldl
            (\_ entity ( entities_, updated ) ->
                let
                    ( addresses, updated_ ) =
                        updateAddressLinkForAddresses id ( neighbor, address ) entity.addresses
                in
                if updated_ then
                    ( Dict.insert entity.id
                        { entity
                            | addresses = addresses
                        }
                        entities_
                    , True
                    )

                else
                    ( entities_, updated )
            )
            ( entities, False )


updateAddressLinkForAddresses : AddressId -> ( Api.Data.NeighborAddress, Address ) -> Dict AddressId Address -> ( Dict AddressId Address, Bool )
updateAddressLinkForAddresses id neighbor addresses =
    case Dict.get id addresses of
        Nothing ->
            ( addresses, False )

        Just found ->
            ( Dict.insert found.id
                { found
                    | links = insertAddressLink neighbor found.links
                }
                addresses
            , True
            )


updateEntityLinks : { currency : String, entity : Int } -> List ( Api.Data.NeighborEntity, Entity ) -> IntDict Layer -> IntDict Layer
updateEntityLinks { currency, entity } neighbors layers =
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
                                                insertEntityLinks relevant found.links
                                        }
                                        layer.entities
                            }
            )
        )
        ( neighbors, layers )
        layers
        |> second


insertAddressLink : ( Api.Data.NeighborAddress, Address ) -> Address.Links -> Address.Links
insertAddressLink ( link, address ) (Address.Links links) =
    Dict.insert address.id
        { value = link.value
        , noTxs = link.noTxs
        , labels = link.labels
        , node = address
        }
        links
        |> Address.Links


insertEntityLinks : List ( Api.Data.NeighborEntity, Entity ) -> Entity.Links -> Entity.Links
insertEntityLinks neighbors (Entity.Links links) =
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
        updatedEntities =
            Set.toList updatedIds
                |> List.filterMap (\e -> Layer.getEntity e layers)
    in
    layers
        |> IntDict.foldl
            (\_ layer layers_ ->
                let
                    relevant =
                        updatedEntities
                            |> List.filter (.id >> Id.layer >> (<) layer.id)

                    ( entities, updated ) =
                        syncLinksOnEntities layer.entities relevant
                in
                if updated then
                    IntDict.insert layer.id
                        { layer
                            | entities = entities
                        }
                        layers_

                else
                    layers_
            )
            layers


syncLinksOnEntities : Dict EntityId Entity -> List Entity -> ( Dict EntityId Entity, Bool )
syncLinksOnEntities entities relevant =
    entities
        |> Dict.foldl
            (\_ entity ( entities_, updated ) ->
                let
                    ( entity_, updated_ ) =
                        syncLinksOnEntity entity relevant
                in
                if updated_ then
                    ( Dict.insert entity.id entity_ entities_, True )

                else
                    ( entities_, updated )
            )
            ( entities, False )


syncLinksOnEntity : Entity -> List Entity -> ( Entity, Bool )
syncLinksOnEntity entity relevant =
    relevant
        |> List.foldl
            (\updEnt ( entity_, updated ) ->
                case entity_.links of
                    Entity.Links links ->
                        case Dict.get updEnt.id links of
                            Nothing ->
                                ( entity_, updated )

                            Just link ->
                                ( { entity_
                                    | links =
                                        Dict.insert updEnt.id { link | node = updEnt } links
                                            |> Entity.Links
                                    , addresses =
                                        syncLinksOnAddresses entity_.addresses updEnt.addresses
                                  }
                                , True
                                )
            )
            ( entity, False )


syncLinksOnAddresses : Dict AddressId Address -> Dict AddressId Address -> Dict AddressId Address
syncLinksOnAddresses sources targets =
    sources
        |> Dict.foldl
            (\src source sources_ ->
                sources_
                    |> Dict.insert src
                        { source
                            | links =
                                Address.Links <|
                                    case source.links of
                                        Address.Links links ->
                                            links
                                                |> Dict.foldl
                                                    (\tgt link links_ ->
                                                        case Dict.get tgt targets of
                                                            Nothing ->
                                                                links_

                                                            Just found ->
                                                                links_
                                                                    |> Dict.insert tgt
                                                                        { link | node = found }
                                                    )
                                                    links
                        }
            )
            sources


updateAddress : AddressId -> (Address -> Address) -> IntDict Layer -> IntDict Layer
updateAddress id update =
    IntDict.update (Id.layer id) (Maybe.map (updateAddressOnLayer id update))


updateAddressOnLayer : AddressId -> (Address -> Address) -> Layer -> Layer
updateAddressOnLayer id update layer =
    { layer
        | entities =
            layer.entities
                |> Dict.foldl
                    (\_ entity entities ->
                        if Dict.member id entity.addresses then
                            Dict.insert
                                entity.id
                                { entity
                                    | addresses = Dict.update id (Maybe.map update) entity.addresses
                                }
                                entities

                        else
                            entities
                    )
                    layer.entities
    }


updateAddresses : { currency : String, address : String } -> (Address -> Address) -> IntDict Layer -> IntDict Layer
updateAddresses { currency, address } update layers =
    layers
        |> IntDict.foldl
            (\_ layer layers_ ->
                let
                    addressId =
                        Id.initAddressId { currency = currency, id = address, layer = layer.id }

                    ( entities, updated ) =
                        updateAddressesForEntities addressId update layer.entities
                in
                if Debug.log ("Updated layer " ++ String.fromInt layer.id) updated then
                    IntDict.insert layer.id
                        { layer | entities = entities }
                        layers_

                else
                    layers_
            )
            layers


updateAddressesForEntities : AddressId -> (Address -> Address) -> Dict EntityId Entity -> ( Dict EntityId Entity, Bool )
updateAddressesForEntities id update entities =
    entities
        |> Dict.foldl
            (\_ entity ( entities_, updated ) ->
                let
                    ( addresses, updated_ ) =
                        updateAddressesForAddresses id update entity.addresses
                in
                if Debug.log ("updated entity " ++ Debug.toString entity.id) updated_ then
                    ( Dict.insert entity.id
                        { entity
                            | addresses = addresses
                        }
                        entities_
                    , True
                    )

                else
                    ( entities_, updated )
            )
            ( entities, False )


updateAddressesForAddresses : AddressId -> (Address -> Address) -> Dict AddressId Address -> ( Dict AddressId Address, Bool )
updateAddressesForAddresses id update addresses =
    case Dict.get id addresses of
        Nothing ->
            ( addresses, False )

        Just found ->
            ( Dict.insert found.id
                (update found)
                addresses
            , True
            )
