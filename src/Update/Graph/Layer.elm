module Update.Graph.Layer exposing
    ( Acc
    , addAddress
    , addAddressAtEntity
    , addEntitiesAt
    , addEntity
    , addEntityNeighbors
    , anchorsToPositions
    , deserialize
    , moveEntity
    , releaseEntity
    , removeAddress
    , removeAddressLinksTo
    , removeEntity
    , syncLinks
    , updateAddress
    , updateAddressLink
    , updateAddresses
    , updateEntities
    , updateEntity
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
import Model.Entity as E
import Model.Graph exposing (DeserializedAddress, Deserializing)
import Model.Graph.Address as Address exposing (Address)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id as Id exposing (AddressId, EntityId)
import Model.Graph.Layer as Layer exposing (Layer)
import Model.Graph.Link exposing (..)
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
addEntity : Plugins -> Update.Config -> Dict String Color -> Api.Data.Entity -> IntDict Layer -> Acc EntityId
addEntity plugins uc colors entity layers =
    addEntitiesAt plugins
        uc
        (anchorsToPositions Nothing layers)
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


{-| Add neighbors next to an entity. Also insert placeholder links
-}
addEntityNeighbors : Plugins -> Update.Config -> Entity -> Bool -> Dict String Color -> List Api.Data.Entity -> IntDict Layer -> Acc EntityId
addEntityNeighbors plugins uc entity isOutgoing colors neighbors layers =
    let
        added =
            addEntitiesAt plugins
                uc
                (anchorsToPositions (IntDict.singleton (Id.layer entity.id) ( entity, isOutgoing ) |> Just) layers)
                neighbors
                { layers = layers
                , new = Set.empty
                , colors = colors
                , repositioned = Set.empty
                }

        layers_ =
            if isOutgoing then
                let
                    pseudoLinks =
                        added.new
                            |> Set.toList
                            |> List.filterMap
                                (\e -> Layer.getEntity e added.layers)
                            |> List.map (pair PlaceholderLinkData)
                in
                updateEntity entity.id
                    (\e ->
                        { e
                            | links = insertEntityLinks pseudoLinks e.links
                        }
                    )
                    added.layers

            else
                added.new
                    |> Set.toList
                    |> List.foldl
                        (\new layers__ ->
                            updateEntity new
                                (\e ->
                                    { e
                                        | links = insertEntityLinks [ ( PlaceholderLinkData, entity ) ] e.links
                                    }
                                )
                                layers__
                        )
                        added.layers
    in
    { added
        | layers = layers_
    }


anchorsToPositions : Maybe (IntDict ( Entity, Bool )) -> IntDict Layer -> IntDict Position
anchorsToPositions anchors layers =
    case anchors of
        Nothing ->
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

        Just anchs ->
            anchs
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
                                                        Layer.getX l
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


addEntitiesAt : Plugins -> Update.Config -> IntDict Position -> List Api.Data.Entity -> Acc EntityId -> Acc EntityId
addEntitiesAt plugins uc positions entities acc =
    IntDict.foldl
        (\layerId position acc_ ->
            IntDict.get layerId acc_.layers
                |> Maybe.withDefault (Layer.init position.x layerId)
                |> (\layer ->
                        let
                            accToLayer =
                                List.foldl
                                    (\entity ->
                                        addEntityHere plugins uc position entity
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


addEntityHere : Plugins -> Update.Config -> Position -> Api.Data.Entity -> AccEntity -> AccEntity
addEntityHere plugins uc position entity { layer, colors, new, repositioned } =
    let
        entityId =
            Id.initEntityId { currency = entity.currency, id = entity.entity, layer = layer.id }

        ( ( newEntities, newRepositioned ), newEntity ) =
            case Dict.get entityId layer.entities of
                Just ent ->
                    ( ( Dict.update entityId
                            (Maybe.map (\e -> { e | entity = entity }))
                            layer.entities
                      , Set.empty
                      )
                    , ent
                    )

                Nothing ->
                    let
                        leftBound =
                            Layer.getRightBound layer

                        rightBound =
                            Layer.getLeftBound layer

                        newEnt =
                            Entity.init
                                plugins
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
            )
        |> Maybe.withDefault layers


releaseEntity : EntityId -> IntDict Layer -> IntDict Layer
releaseEntity id layers =
    updateEntity id Entity.release layers


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


updateAddressLink : { currency : String, address : String } -> ( LinkData, Address ) -> IntDict Layer -> IntDict Layer
updateAddressLink { currency, address } ( neighbor, target ) layers =
    layers
        |> IntDict.foldl
            (\_ layer layers_ ->
                if layer.id /= Id.layer target.id - 1 then
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


updateAddressLinkForEntities : AddressId -> ( LinkData, Address ) -> Dict EntityId Entity -> ( Dict EntityId Entity, Bool )
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


updateAddressLinkForAddresses : AddressId -> ( LinkData, Address ) -> Dict AddressId Address -> ( Dict AddressId Address, Bool )
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


updateEntityLinks : { currency : String, entity : Int } -> List ( LinkData, Entity ) -> IntDict Layer -> IntDict Layer
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


insertAddressLink : ( LinkData, Address ) -> Address.Links -> Address.Links
insertAddressLink ( link, address ) (Address.Links links) =
    Dict.update address.id
        (Maybe.withDefault
            { link = link
            , node = address
            }
            >> Just
        )
        links
        |> Address.Links


insertEntityLinks : List ( LinkData, Entity ) -> Entity.Links -> Entity.Links
insertEntityLinks neighbors (Entity.Links links) =
    neighbors
        |> List.foldl
            (\( link, entity ) li ->
                Dict.update entity.id
                    (Maybe.map
                        (\l ->
                            { l
                                | link = link
                                , node = entity
                            }
                        )
                        >> Maybe.withDefault
                            { link = link
                            , node = entity
                            }
                        >> Just
                    )
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
                if updated then
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


updateEntity : EntityId -> (Entity -> Entity) -> IntDict Layer -> IntDict Layer
updateEntity id update =
    IntDict.update (Id.layer id) (Maybe.map (updateEntityOnLayer id update))


updateEntityOnLayer : EntityId -> (Entity -> Entity) -> Layer -> Layer
updateEntityOnLayer id update layer =
    { layer
        | entities = Dict.update id (Maybe.map update) layer.entities
    }


updateEntities : E.Entity -> (Entity -> Entity) -> IntDict Layer -> IntDict Layer
updateEntities { currency, entity } update layers =
    layers
        |> IntDict.foldl
            (\_ layer layers_ ->
                let
                    entityId =
                        Id.initEntityId { currency = currency, id = entity, layer = layer.id }

                    ( entities, updated ) =
                        case Dict.get entityId layer.entities of
                            Nothing ->
                                ( layer.entities, False )

                            Just found ->
                                ( Dict.insert found.id
                                    (update found)
                                    layer.entities
                                , True
                                )
                in
                if updated then
                    IntDict.insert layer.id
                        { layer | entities = entities }
                        layers_

                else
                    layers_
            )
            layers


updateEntitiesIf : (Entity -> Bool) -> (Entity -> Entity) -> IntDict Layer -> IntDict Layer
updateEntitiesIf predicate update layers =
    layers
        |> IntDict.foldl
            (\_ layer layers_ ->
                let
                    ( entities, updated ) =
                        layer.entities
                            |> Dict.foldl
                                (\k entity ( acc, upd ) ->
                                    if predicate entity then
                                        ( Dict.update k (Maybe.map update) acc
                                        , True
                                        )

                                    else
                                        ( acc, upd )
                                )
                                ( layer.entities, False )
                in
                if updated then
                    IntDict.insert layer.id
                        { layer | entities = entities }
                        layers_

                else
                    layers_
            )
            layers


removeEntityLinksTo : EntityId -> IntDict Layer -> IntDict Layer
removeEntityLinksTo id layers =
    updateEntitiesIf
        (\a ->
            case a.links of
                Entity.Links links ->
                    Dict.member id links
        )
        (\a ->
            { a
                | links =
                    case a.links of
                        Entity.Links links ->
                            Dict.remove id links
                                |> Entity.Links
            }
        )
        layers


removeAddressLinksTo : AddressId -> IntDict Layer -> IntDict Layer
removeAddressLinksTo id layers =
    updateAddressesIf
        (\a ->
            case a.links of
                Address.Links links ->
                    Dict.member id links
        )
        (\a ->
            { a
                | links =
                    case a.links of
                        Address.Links links ->
                            Dict.remove id links
                                |> Address.Links
            }
        )
        layers


updateAddressesIf : (Address -> Bool) -> (Address -> Address) -> IntDict Layer -> IntDict Layer
updateAddressesIf predicate update layers =
    layers
        |> IntDict.foldl
            (\_ layer layers_ ->
                let
                    ( entities, updated ) =
                        updateAddressesForEntitiesIf predicate update layer.entities
                in
                if updated then
                    IntDict.insert layer.id
                        { layer | entities = entities }
                        layers_

                else
                    layers_
            )
            layers


updateAddressesForEntitiesIf : (Address -> Bool) -> (Address -> Address) -> Dict EntityId Entity -> ( Dict EntityId Entity, Bool )
updateAddressesForEntitiesIf predicate update entities =
    entities
        |> Dict.foldl
            (\_ entity ( entities_, updated ) ->
                let
                    ( addresses, updated_ ) =
                        updateAddressesForAddressesIf predicate update entity.addresses
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


updateAddressesForAddressesIf : (Address -> Bool) -> (Address -> Address) -> Dict AddressId Address -> ( Dict AddressId Address, Bool )
updateAddressesForAddressesIf predicate update addresses =
    addresses
        |> Dict.foldl
            (\k address ( acc, updated ) ->
                if predicate address then
                    ( Dict.update k (Maybe.map update) acc
                    , True
                    )

                else
                    ( acc, updated )
            )
            ( addresses, False )


removeAddress : Id.AddressId -> IntDict Layer -> IntDict Layer
removeAddress id layers =
    Layer.getAddress id layers
        |> Maybe.map
            (\address ->
                updateEntity address.entityId (\e -> { e | addresses = Dict.remove id e.addresses }) layers
                    |> syncLinks (Set.singleton address.entityId)
            )
        |> Maybe.map (removeAddressLinksTo id)
        |> Maybe.withDefault layers


removeEntity : Id.EntityId -> IntDict Layer -> IntDict Layer
removeEntity id layers =
    Layer.getEntity id layers
        |> Maybe.map
            (\entity ->
                entity.addresses
                    |> Dict.keys
                    |> List.foldl removeAddress layers
            )
        |> Maybe.map
            (IntDict.update (Id.layer id)
                (Maybe.map
                    (\l -> { l | entities = Dict.remove id l.entities })
                )
            )
        |> Maybe.map (removeEntityLinksTo id)
        |> Maybe.withDefault layers


deserialize : Plugins -> Update.Config -> Deserializing -> { layers : IntDict Layer, colors : Dict String Color }
deserialize plugins uc { deserialized, addresses, entities } =
    let
        entitiesDict : Dict ( String, Int ) Api.Data.Entity
        entitiesDict =
            entities
                |> List.foldl
                    (\entity ->
                        Dict.insert ( entity.currency, entity.entity ) entity
                    )
                    Dict.empty

        addressesDict : Dict ( String, String ) Api.Data.Address
        addressesDict =
            addresses
                |> List.foldl
                    (\address ->
                        Dict.insert ( address.currency, address.address ) address
                    )
                    Dict.empty

        addressNodesByLayer : List (List DeserializedAddress)
        addressNodesByLayer =
            deserialized.addresses
                |> List.Extra.gatherEqualsBy (.id >> Id.layer)
                |> List.sortBy (first >> .id >> Id.layer)
                |> List.map (\( fst, more ) -> fst :: more)
                |> Debug.log "addressNodesByLayer"

        entityByAddress : Dict ( String, String ) Int
        entityByAddress =
            addresses
                |> List.foldl
                    (\{ currency, address, entity } ->
                        Dict.insert ( currency, address ) entity
                    )
                    Dict.empty
                |> Debug.log "entityByAddress"

        addressNodesByLayerWithEntity : List ( Int, List ( DeserializedAddress, Int ) )
        addressNodesByLayerWithEntity =
            addressNodesByLayer
                |> List.indexedMap
                    (\i addrs ->
                        addrs
                            |> List.filterMap
                                (\address ->
                                    Dict.get ( Id.currency address.id, Id.addressId address.id ) entityByAddress
                                        |> Maybe.map (pair address)
                                )
                            |> pair i
                    )

        entitiesWithPositionByLayer : List ( Int, List ( Api.Data.Entity, Position ) )
        entitiesWithPositionByLayer =
            addressNodesByLayerWithEntity
                |> List.map
                    (mapSecond
                        (List.Extra.gatherEqualsBy second
                            >> List.map (\( fst, more ) -> ( second fst, first fst :: List.map first more ))
                            >> List.filterMap
                                (\( e, addrs ) ->
                                    List.Extra.find (.entity >> (==) e) entities
                                        |> Maybe.andThen
                                            (\entity ->
                                                addressesToPosition addrs
                                                    |> Maybe.map (pair entity)
                                            )
                                )
                            >> (++)
                                (deserialized.entities
                                    |> List.filterMap
                                        (\e ->
                                            List.Extra.find
                                                (\entity ->
                                                    entity.entity
                                                        == Id.entityId e.id
                                                        && entity.currency
                                                        == Id.currency e.id
                                                )
                                                entities
                                                |> Maybe.map
                                                    (\entity -> ( entity, Position e.x e.y ))
                                        )
                                )
                        )
                    )

        _ =
            entitiesWithPositionByLayer
                |> List.map (\( i, list ) -> ( i, List.map (\( e, p ) -> ( e.entity, p )) list ))
                |> Debug.log "entitiesWithPositionByLayer"

        addressesToPosition : List DeserializedAddress -> Maybe Position
        addressesToPosition addrs =
            List.Extra.minimumBy .y addrs
                |> Maybe.map (\{ x, y } -> Position x y)

        entitiesAdded =
            entitiesWithPositionByLayer
                |> List.foldl
                    (\( i, entitiesWithPosition ) acc ->
                        entitiesWithPosition
                            |> List.foldl
                                (\( entity, position ) ->
                                    addEntitiesAt plugins
                                        uc
                                        (IntDict.singleton i position)
                                        [ entity ]
                                )
                                acc
                    )
                    { layers = IntDict.empty
                    , colors = Dict.empty
                    , new = Set.empty
                    , repositioned = Set.empty
                    }
    in
    addressNodesByLayerWithEntity
        |> List.foldl
            (\( layerId, addressesWithEntity ) acc ->
                addressesWithEntity
                    |> List.filterMap
                        (\( { id }, e ) ->
                            Dict.get ( Id.currency id, Id.addressId id ) addressesDict
                                |> Maybe.map (\a -> ( a, e ))
                        )
                    |> List.foldl
                        (\( address, e ) acc_ ->
                            let
                                entityId =
                                    Id.initEntityId
                                        { currency = address.currency
                                        , id = e
                                        , layer = layerId
                                        }
                            in
                            addAddressAtEntity
                                plugins
                                uc
                                acc_.colors
                                entityId
                                address
                                acc_.layers
                        )
                        acc
            )
            { layers = entitiesAdded.layers
            , colors = entitiesAdded.colors
            , new = Set.empty
            , repositioned = Set.empty
            }
        |> (\{ layers, colors } -> { layers = layers, colors = colors })
