module Graph.Update.Layer exposing (addAddress, addEntity)

import Api.Data
import Graph.Model exposing (..)
import Graph.Model.Id exposing (AddressId, EntityId)
import Graph.Model.Layer exposing (Layer)
import Graph.Update.Entity as Entity


type alias Added id =
    { layers : List Layer
    , new : List id
    }


addAddress : Api.Data.Address -> List Layer -> Added AddressId
addAddress address layers =
    addAddressHelp address layers { layers = [], new = [] }


addEntity : Api.Data.Entity -> List Layer -> Added EntityId -> Added EntityId
addEntity entity layers added =
    { layers = layers
    , new = []
    }


addAddressHelp : Api.Data.Address -> List Layer -> Added AddressId -> Added AddressId
addAddressHelp address layers added =
    case layers of
        layer :: rest ->
            let
                {- { entities, new } -}
                addedEntity =
                    Entity.addAddress address layer.entities
            in
            addAddressHelp
                address
                rest
                { layers = added.layers ++ [ { layer | entities = addedEntity.entities } ]
                , new = added.new ++ addedEntity.new
                }

        [] ->
            added
