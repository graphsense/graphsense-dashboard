module Update.Graph.Entity exposing (addAddress, move, release, updateEntity)

import Api.Data
import Color exposing (Color)
import Config.Update as Update
import Dict exposing (Dict)
import Init.Graph.Address as Address
import List.Extra as List
import Model.Graph.Address exposing (..)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity exposing (..)
import Model.Graph.Id as Id exposing (..)
import Update.Graph.Address as Address
import Update.Graph.Color as Color


type alias Added =
    { entities : List Entity
    , new : List AddressId
    , colors : Dict String Color
    }


addAddress : Update.Config -> Dict String Color -> Api.Data.Address -> List Entity -> Added
addAddress uc colors address entities =
    addAddressHelp uc address entities { entities = [], new = [], colors = colors }


addAddressHelp : Update.Config -> Api.Data.Address -> List Entity -> Added -> Added
addAddressHelp uc address entities added =
    case entities of
        entity :: rest ->
            case addAddressToEntity uc added.colors address entity of
                Nothing ->
                    addAddressHelp uc
                        address
                        rest
                        { added
                            | entities = added.entities ++ [ entity ]
                        }

                Just { updatedEntity, new, colors } ->
                    { added
                        | entities =
                            added.entities
                                ++ updatedEntity
                                :: rest
                        , new = new :: added.new
                        , colors = colors
                    }

        [] ->
            added


addAddressToEntity : Update.Config -> Dict String Color -> Api.Data.Address -> Entity -> Maybe { updatedEntity : Entity, new : AddressId, colors : Dict String Color }
addAddressToEntity uc colors address entity =
    if address.entity == Id.entityId entity.id && address.currency == Id.currency entity.id then
        List.find (.id >> Id.id >> (==) address.address) entity.addresses
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
                            entity.addresses ++ [ newAddress ]
                    }
                 , new = newAddress.id
                 , colors = newColors
                 }
                )
            |> Just

    else
        Nothing


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


move : Coords -> Entity -> ( Entity, () )
move vector entity =
    ( { entity
        | dx = vector.x
        , dy = vector.y
        , addresses =
            List.map (Address.move vector) entity.addresses
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
            List.map Address.release entity.addresses
      }
    , ()
    )
