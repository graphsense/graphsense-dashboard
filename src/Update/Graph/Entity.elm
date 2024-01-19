module Update.Graph.Entity exposing
    ( BoundingBox
    , addAddress
    , insertEntityShadowLink
    , move
    , release
    , repositionAddresses
    , repositionAround
    , updateColor
    )

import Api.Data
import Color exposing (Color)
import Config.Graph exposing (padding)
import Config.Update as Update
import Dict exposing (Dict)
import Init.Graph.Address as Address
import Init.Graph.Id as Id
import List.Extra
import Log
import Model.Graph.Address as Address exposing (..)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (..)
import Model.Graph.Id as Id exposing (..)
import Model.Graph.Link as Link
import Plugin.Update exposing (Plugins)
import Set exposing (Set)
import Tuple exposing (..)
import Update.Graph.Address as Address
import Update.Graph.Color as Color


type alias Acc =
    { entities : Dict EntityId Entity
    , new : Set AddressId
    , colors : Dict String Color
    , repositioned : Set EntityId
    }


addAddress : Plugins -> Update.Config -> Int -> Api.Data.Address -> Acc -> Acc
addAddress plugins uc layerId address acc =
    let
        entityId =
            Id.initEntityId { layer = layerId, currency = address.currency, id = address.entity }
    in
    Dict.get entityId acc.entities
        |> Maybe.andThen (addAddressToEntity plugins uc acc.colors address)
        |> Maybe.map
            (\newAcc ->
                let
                    ( entities, repositioned ) =
                        Dict.insert entityId newAcc.updatedEntity acc.entities
                            |> repositionAround newAcc.updatedEntity
                in
                { entities = entities
                , new = Set.insert newAcc.new acc.new
                , colors = newAcc.colors
                , repositioned =
                    Set.union repositioned acc.repositioned
                        |> Set.insert newAcc.updatedEntity.id
                }
            )
        |> Maybe.withDefault acc


addAddressToEntity : Plugins -> Update.Config -> Dict String Color -> Api.Data.Address -> Entity -> Maybe { updatedEntity : Entity, new : AddressId, colors : Dict String Color }
addAddressToEntity plugins uc colors address entity =
    if Dict.member (Id.initAddressId { layer = Id.layer entity.id, id = address.address, currency = address.currency }) entity.addresses then
        Nothing

    else
        let
            newAddress =
                Address.init plugins entity address

            newColors =
                Color.update uc colors newAddress.category
        in
        { updatedEntity =
            { entity
                | addresses =
                    Dict.insert newAddress.id newAddress entity.addresses
            }
        , new = newAddress.id
        , colors = newColors
        }
            |> Just


type alias BoundingBox =
    { left : Maybe Float
    , right : Maybe Float
    , lower : Maybe Float
    , upper : Maybe Float
    }


toLeftBound : BoundingBox -> Entity -> Float -> Float
toLeftBound { left } entity x =
    left
        |> Maybe.map
            (\l ->
                x
                    + (l
                        - (entity.x + x)
                        |> max 0
                      )
            )
        |> Maybe.withDefault x


toRightBound : BoundingBox -> Entity -> Float -> Float
toRightBound { right } entity x =
    right
        |> Maybe.map
            (\r ->
                x
                    - ((entity.x + Entity.getWidth entity + x)
                        - r
                        |> max 0
                      )
            )
        |> Maybe.withDefault x


move : BoundingBox -> Coords -> Entity -> Entity
move bb vector entity =
    let
        v =
            { x =
                toLeftBound bb entity vector.x
                    |> toRightBound bb entity
            , y = vector.y
            }
    in
    { entity
        | dx = v.x
        , dy = v.y
        , addresses =
            Dict.map (\_ -> Address.move v) entity.addresses
    }


release : Entity -> Entity
release entity =
    { entity
        | x = entity.x + entity.dx
        , y = entity.y + entity.dy
        , dx = 0
        , dy = 0
        , addresses =
            Dict.map (\_ -> Address.release) entity.addresses
    }


translate : Coords -> Entity -> Entity
translate vector entity =
    { entity
        | x = entity.x + vector.x
        , y = entity.y + vector.y
        , addresses =
            Dict.map (\_ -> Address.translate vector) entity.addresses
    }


repositionAround : Entity -> Dict EntityId Entity -> ( Dict EntityId Entity, Set EntityId )
repositionAround pivot entities =
    let
        sorted =
            Dict.values entities
                |> List.sortBy Entity.getY
                |> List.filter (.id >> (/=) pivot.id)

        ( upper, lower ) =
            sorted
                |> List.Extra.splitWhen
                    (Entity.getY >> (<=) (Entity.getY pivot))
                |> Maybe.withDefault ( sorted, [] )
                |> mapFirst List.reverse

        reposition p isUpper nodes ready repositioned =
            case nodes of
                nearest :: rest ->
                    let
                        py =
                            Entity.getY p

                        ph =
                            Entity.getHeight p

                        ny =
                            Entity.getY nearest

                        nh =
                            Entity.getHeight nearest
                    in
                    if py + ph >= ny && py <= ny + nh |> Log.log2 "overlapping below" then
                        let
                            newEntity =
                                translate { x = 0, y = (py + ph - ny) + padding } nearest
                        in
                        reposition newEntity isUpper rest (p :: ready) (Set.insert newEntity.id repositioned)

                    else if py >= ny && py <= ny + nh |> Log.log2 "overlapping above" then
                        let
                            newEntity =
                                translate { x = 0, y = -(ny + nh - py) - padding } nearest
                        in
                        reposition newEntity isUpper rest (p :: ready) (Set.insert newEntity.id repositioned)

                    else
                        ( p :: ready ++ (nearest :: rest), repositioned )

                [] ->
                    ( p :: ready, repositioned )

        ( newUpper, repos1 ) =
            reposition pivot True upper [] Set.empty

        ( newLower, repos2 ) =
            reposition pivot False lower [] Set.empty
    in
    ( newUpper
        ++ newLower
        |> List.map (\e -> ( e.id, e ))
        |> (\l ->
                l
           )
        |> Dict.fromList
    , Set.union repos1 repos2
    )


insertEntityShadowLink : Entity -> Entity -> Entity
insertEntityShadowLink target source =
    case source.shadowLinks of
        Entity.Links links ->
            { source
                | shadowLinks =
                    Dict.insert target.id
                        { node = target
                        , forceShow = False
                        , link = Link.PlaceholderLinkData
                        , selected = False
                        }
                        links
                        |> Entity.Links
                , addresses = insertAddressesShadowLinks target source
            }


insertAddressesShadowLinks : Entity -> Entity -> Dict AddressId Address
insertAddressesShadowLinks target source =
    let
        ( updatedAddresses, updated ) =
            source.addresses
                |> Dict.foldl
                    (\sourceId sourceAddress ( updatedAddresses_, updated_ ) ->
                        let
                            id =
                                Id.initAddressId
                                    { currency = Id.currency sourceId
                                    , layer = Id.layer target.id
                                    , id = Id.addressId sourceId
                                    }
                        in
                        Dict.get id target.addresses
                            |> Maybe.map
                                (\targetAddress ->
                                    ( Dict.insert sourceId
                                        { sourceAddress
                                            | shadowLinks =
                                                case sourceAddress.shadowLinks of
                                                    Address.Links links ->
                                                        Dict.insert targetAddress.id
                                                            { node = targetAddress
                                                            , forceShow = False
                                                            , link = Link.PlaceholderLinkData
                                                            , selected = False
                                                            }
                                                            links
                                                            |> Address.Links
                                        }
                                        updatedAddresses_
                                    , True
                                    )
                                )
                            |> Maybe.withDefault ( updatedAddresses_, updated_ )
                    )
                    ( source.addresses, False )
    in
    if updated then
        updatedAddresses

    else
        source.addresses


updateColor : Color -> Entity -> Entity
updateColor color entity =
    { entity
        | color =
            if Just (Color.toCssString color) == Maybe.map Color.toCssString entity.color then
                Nothing

            else
                Just color
    }


repositionAddresses : Entity -> Entity
repositionAddresses e =
    e.addresses
        |> Dict.foldl
            (\addressId address entity ->
                { entity
                    | addresses =
                        Dict.insert addressId
                            { address
                                | y = Address.initY entity
                            }
                            entity.addresses
                }
            )
            { e | addresses = Dict.empty }
