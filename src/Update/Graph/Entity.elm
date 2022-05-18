module Update.Graph.Entity exposing (BoundingBox, addAddress, move, release, repositionAround, translate, updateEntity)

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


addAddress : Update.Config -> Int -> Api.Data.Address -> Acc -> Acc
addAddress uc layerId address acc =
    Dict.get (Id.initEntityId { layer = layerId, currency = address.currency, id = address.entity }) acc.entities
        |> Maybe.map
            (\entity ->
                let
                    newAcc =
                        addAddressToEntity uc acc.colors address entity

                    ( entities, repositioned ) =
                        Dict.insert entity.id newAcc.updatedEntity acc.entities
                            |> repositionAround newAcc.updatedEntity
                in
                { entities = entities
                , new = Set.insert newAcc.new acc.new
                , colors = newAcc.colors
                , repositioned = repositioned
                }
            )
        |> Maybe.withDefault acc


addAddressToEntity : Update.Config -> Dict String Color -> Api.Data.Address -> Entity -> { updatedEntity : Entity, new : AddressId, colors : Dict String Color }
addAddressToEntity uc colors address entity =
    Dict.get (Id.initAddressId { layer = Id.layer entity.id, id = address.address, currency = address.currency }) entity.addresses
        |> Maybe.map (\{ id } -> { updatedEntity = entity, new = id, colors = colors })
        |> Maybe.withDefault
            (let
                newAddress =
                    Address.init entity address

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
                        - (Entity.getX entity + x)
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
                    - ((Entity.getX entity + Entity.getWidth entity + x)
                        - r
                        |> max 0
                        |> Debug.log "adapt"
                      )
            )
        |> Maybe.withDefault x


move : BoundingBox -> Coords -> Entity -> ( Entity, () )
move bb vector entity =
    let
        v =
            { x =
                toLeftBound (Debug.log "bb" bb) entity vector.x
                    |> toRightBound bb entity
            , y = vector.y
            }
    in
    ( { entity
        | dx = v.x
        , dy = v.y
        , addresses =
            Dict.map (\_ -> Address.move v) entity.addresses
      }
    , ()
    )


release : Entity -> ( Entity, () )
release entity =
    ( { entity
        | x = entity.x + entity.dx
        , y = entity.y + entity.dy
        , dx = 0
        , dy = 0
        , addresses =
            Dict.map (\_ -> Address.release) entity.addresses
      }
    , ()
    )


translate : Coords -> Entity -> Entity
translate vector entity =
    { entity
        | x = entity.x + vector.x
        , y = entity.y + vector.y
    }


repositionAround : Entity -> Dict EntityId Entity -> ( Dict EntityId Entity, Set EntityId )
repositionAround pivot entities =
    let
        simple e =
            { id = e.id, y = e.y }

        _ =
            Log.log "repositionAround"
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

        ( upper, lower ) =
            sorted
                |> List.Extra.splitWhen
                    (Entity.getY >> (<=) (Entity.getY pivot))
                |> Maybe.withDefault ( sorted, [] )
                |> mapFirst List.reverse

        reposition p nodes ready repositioned =
            case nodes of
                nearest :: rest ->
                    let
                        _ =
                            Log.log
                                "repo"
                                { nearest = nearest.id
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
                    if py + ph >= ny && py + ph <= ny + nh |> Log.log "overlapping below" then
                        let
                            newEntity =
                                translate { x = 0, y = (py + ph - ny) + padding } nearest
                        in
                        reposition newEntity rest (p :: ready) (Set.insert newEntity.id repositioned)

                    else if py >= ny && py <= ny + nh |> Log.log "overlapping above" then
                        let
                            newEntity =
                                translate { x = 0, y = -(ny + nh - py) - padding } nearest
                        in
                        reposition newEntity rest (p :: ready) (Set.insert newEntity.id repositioned)

                    else
                        ( p :: ready ++ (nearest :: rest), repositioned )

                [] ->
                    ( p :: ready, repositioned )

        ( newUpper, repos1 ) =
            reposition pivot upper [] Set.empty

        ( newLower, repos2 ) =
            reposition pivot lower [] Set.empty
    in
    ( newUpper
        ++ newLower
        |> List.map (\e -> ( e.id, e ))
        |> (\l ->
                let
                    _ =
                        Log.log "new" (List.map first l)
                in
                l
           )
        |> Dict.fromList
    , Set.union repos1 repos2
    )
