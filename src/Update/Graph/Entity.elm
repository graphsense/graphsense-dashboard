module Update.Graph.Entity exposing (addAddress)

import Api.Data
import Init.Graph.Address as Address
import List.Extra as List
import Model.Graph.Address exposing (..)
import Model.Graph.Entity exposing (..)
import Model.Graph.Id as Id exposing (..)


type alias Added =
    { entities : List Entity
    , new : List AddressId
    }


type alias AddressesAdded =
    { addresses : List Address
    , new : List AddressId
    }


addAddress : Api.Data.Address -> List Entity -> Added
addAddress address entities =
    addAddressHelp address entities { entities = [], new = [] }


addAddressHelp : Api.Data.Address -> List Entity -> Added -> Added
addAddressHelp address entities added =
    case entities of
        entity :: rest ->
            case addAddressToEntity address entity of
                Nothing ->
                    addAddressHelp address
                        rest
                        { added
                            | entities = added.entities ++ [ entity ]
                        }

                Just { updatedEntity, new } ->
                    { added
                        | entities =
                            added.entities
                                ++ updatedEntity
                                :: rest
                        , new = new :: added.new
                    }

        [] ->
            added


addAddressToEntity : Api.Data.Address -> Entity -> Maybe { updatedEntity : Entity, new : AddressId }
addAddressToEntity address entity =
    if address.entity == Id.entityId entity.id && address.currency == Id.currency entity.id then
        List.find (.id >> Id.id >> (==) address.address) entity.addresses
            |> Maybe.map (\{ id } -> { updatedEntity = entity, new = id })
            |> Maybe.withDefault
                (let
                    newAddress =
                        Address.init entity address
                 in
                 { updatedEntity =
                    { entity
                        | addresses =
                            entity.addresses ++ [ newAddress ]
                    }
                 , new = newAddress.id
                 }
                )
            |> Just

    else
        Nothing
