module Update.Graph.Layer exposing (Added, addAddress, addEntity)

import Api.Data
import Init.Graph.Entity as Entity
import Init.Graph.Layer as Layer
import List.Extra
import Model.Graph exposing (..)
import Model.Graph.Entity as Entity
import Model.Graph.Id exposing (AddressId, EntityId)
import Model.Graph.Layer exposing (Layer)
import Tuple exposing (..)
import Update.Graph.Entity as Entity


type alias Added id =
    { layers : List Layer
    , new : List id
    }


addAddress : Api.Data.Address -> List Layer -> Added AddressId
addAddress address layers =
    addAddressHelp address layers { layers = [], new = [] }


addEntity : Api.Data.Entity -> List Layer -> Added EntityId
addEntity entity layers =
    addEntityHelp entity layers { layers = [], new = [] }


addAddressHelp : Api.Data.Address -> List Layer -> Added AddressId -> Added AddressId
addAddressHelp address layers added =
    case layers of
        layer :: rest ->
            let
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


addEntityHelp : Api.Data.Entity -> List Layer -> Added EntityId -> Added EntityId
addEntityHelp entity layers added =
    case layers of
        layer :: rest ->
            addEntityHelp entity rest { added | layers = added.layers ++ [ layer ] }

        [] ->
            if List.isEmpty added.new then
                let
                    predicate =
                        .id >> (==) 0

                    ( layer, new ) =
                        List.Extra.find predicate added.layers
                            |> Maybe.withDefault (Layer.init 0)
                            |> addEntityHere entity
                in
                { layers =
                    if List.isEmpty added.layers then
                        [ layer ]

                    else
                        List.Extra.setIf predicate layer added.layers
                , new = [ new ]
                }

            else
                added


addEntityHere : Api.Data.Entity -> Layer -> ( Layer, EntityId )
addEntityHere entity layer =
    List.Extra.find (\e -> e.entity.entity == entity.entity && e.entity.currency == entity.currency) layer.entities
        |> Maybe.map (.id >> pair layer)
        |> Maybe.withDefault
            (let
                y =
                    layer.entities
                        |> List.foldl
                            (\e max ->
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
                        |> Maybe.map (\e -> e.y + Entity.calcHeight e)
                        |> Maybe.withDefault 0

                newEntity =
                    Entity.init { layer = layer.id, x = layer.x, y = y } entity
             in
             ( { layer
                | entities =
                    layer.entities ++ [ newEntity ]
               }
             , newEntity.id
             )
            )
