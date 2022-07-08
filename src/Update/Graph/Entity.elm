module Update.Graph.Entity exposing (BoundingBox, addAddress, insertShadowLink, move, release, repositionAround, translate, updateColor, updateEntity)

import Api.Data
import Color exposing (Color)
import Config.Graph as Graph exposing (padding)
import Config.Update as Update
import Dict exposing (Dict)
import Init.Graph.Address as Address
import Init.Graph.Id as Id
import List.Extra
import Log
import Model.Graph.Address exposing (..)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (..)
import Model.Graph.Id as Id exposing (..)
import Model.Graph.Link as Link
import Plugin.Update as Plugin exposing (Plugins)
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
    Dict.get (Id.initEntityId { layer = layerId, currency = address.currency, id = address.entity }) acc.entities
        |> Maybe.map
            (\entity ->
                let
                    newAcc =
                        addAddressToEntity plugins uc acc.colors address entity

                    ( entities, repositioned ) =
                        Dict.insert entity.id newAcc.updatedEntity acc.entities
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


addAddressToEntity : Plugins -> Update.Config -> Dict String Color -> Api.Data.Address -> Entity -> { updatedEntity : Entity, new : AddressId, colors : Dict String Color }
addAddressToEntity plugins uc colors address entity =
    Dict.get (Id.initAddressId { layer = Id.layer entity.id, id = address.address, currency = address.currency }) entity.addresses
        |> Maybe.map (\{ id } -> { updatedEntity = entity, new = id, colors = colors })
        |> Maybe.withDefault
            (let
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
            )


updateEntity : EntityId -> (Entity -> ( Entity, a )) -> List Entity -> List Entity -> ( List Entity, Maybe a )
updateEntity id update entities newEntities =
    case entities of
        entity :: rest ->
            if id == entity.id then
                let
                    ( updatedEntity, newA ) =
                        update entity
                in
                ( newEntities
                    ++ [ updatedEntity ]
                    ++ rest
                , Just newA
                )

            else
                updateEntity id update rest <| newEntities ++ [ entity ]

        [] ->
            ( newEntities, Nothing )


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
        simple e =
            { id = e.id, y = e.y }

        _ =
            Log.log2 "repositionAround"
                { pivot = pivot.id
                , entities =
                    Dict.values entities
                        |> List.map simple
                , upper = List.map simple upper
                , lower = List.map simple lower
                }

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
                        _ =
                            Log.log2
                                "repo"
                                { nearest = nearest.id
                                , p = p.id
                                , py = py
                                , ph = ph
                                , ny = ny
                                , nh = nh
                                }

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
                let
                    _ =
                        Log.log2 "new" (List.map first l)
                in
                l
           )
        |> Dict.fromList
    , Set.union repos1 repos2
    )


insertShadowLink : Entity -> Entity -> Entity
insertShadowLink target source =
    case source.shadowLinks of
        Entity.Links links ->
            { source
                | shadowLinks =
                    Dict.insert target.id
                        { node = target
                        , link = Link.PlaceholderLinkData
                        }
                        links
                        |> Entity.Links
            }


updateColor : Color -> Entity -> Entity
updateColor color entity =
    { entity
        | color =
            if Just (Color.toCssString color) == Maybe.map Color.toCssString entity.color then
                Nothing

            else
                Just color
    }
