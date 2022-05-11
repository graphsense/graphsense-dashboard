module Update.Graph.Layer exposing (Added, addAddress, addEntity, moveEntity, releaseEntity)

import Api.Data
import Color exposing (Color)
import Config.Graph as Graph exposing (padding)
import Config.Update as Update
import Dict exposing (Dict)
import Init.Graph.Entity as Entity
import Init.Graph.Layer as Layer
import List.Extra
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity as Entity exposing (Entity)
import Model.Graph.Id exposing (AddressId, EntityId)
import Model.Graph.Layer exposing (Layer)
import Tuple exposing (..)
import Update.Graph.Color as Color
import Update.Graph.Entity as Entity


type alias Added id =
    { layers : List Layer
    , new : List id
    , colors : Dict String Color
    }


addAddress : Update.Config -> Dict String Color -> Api.Data.Address -> List Layer -> Added AddressId
addAddress uc colors address layers =
    addAddressHelp uc address layers { layers = [], new = [], colors = colors }


addEntity : Update.Config -> Dict String Color -> Api.Data.Entity -> List Layer -> Added EntityId
addEntity uc colors entity layers =
    addEntityHelp uc entity layers { layers = [], new = [], colors = colors }


addAddressHelp : Update.Config -> Api.Data.Address -> List Layer -> Added AddressId -> Added AddressId
addAddressHelp uc address layers added =
    case layers of
        layer :: rest ->
            let
                addedEntity =
                    Entity.addAddress uc added.colors address layer.entities
            in
            addAddressHelp
                uc
                address
                rest
                { layers = added.layers ++ [ { layer | entities = addedEntity.entities } ]
                , new = added.new ++ addedEntity.new
                , colors = addedEntity.colors
                }

        [] ->
            added


addEntityHelp : Update.Config -> Api.Data.Entity -> List Layer -> Added EntityId -> Added EntityId
addEntityHelp uc entity layers added =
    case layers of
        layer :: rest ->
            addEntityHelp uc entity rest { added | layers = added.layers ++ [ layer ] }

        [] ->
            if List.isEmpty added.new then
                let
                    predicate =
                        .id >> (==) 0

                    ( layer, new, colors ) =
                        List.Extra.find predicate added.layers
                            |> Maybe.withDefault (Layer.init 0)
                            |> addEntityHere uc added.colors entity
                in
                { layers =
                    if List.isEmpty added.layers then
                        [ layer ]

                    else
                        List.Extra.setIf predicate layer added.layers
                , new = [ new ]
                , colors = colors
                }

            else
                added


addEntityHere : Update.Config -> Dict String Color -> Api.Data.Entity -> Layer -> ( Layer, EntityId, Dict String Color )
addEntityHere uc colors entity layer =
    List.Extra.find (\e -> e.entity.entity == entity.entity && e.entity.currency == entity.currency) layer.entities
        |> Maybe.map (\e -> ( layer, e.id, colors ))
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
                        |> Maybe.map (\e -> e.y + Entity.getHeight e + padding)
                        |> Maybe.withDefault 0

                newEntity =
                    Entity.init { layer = layer.id, x = layer.x, y = y } entity

                newColors =
                    Color.update uc colors newEntity.category
             in
             ( { layer
                | entities =
                    layer.entities ++ [ newEntity ]
               }
             , newEntity.id
             , newColors
             )
            )


moveEntity : EntityId -> Coords -> List Layer -> List Layer
moveEntity id vector layers =
    updateEntity id (Entity.move vector) layers []
        |> first


releaseEntity : EntityId -> List Layer -> List Layer
releaseEntity id layers =
    updateEntity id Entity.release layers []
        |> first


updateEntity : EntityId -> (Entity -> ( Entity, a )) -> List Layer -> List Layer -> ( List Layer, Maybe a )
updateEntity id update layers newLayers =
    case layers of
        layer :: rest ->
            let
                ( newEntities, maybeA ) =
                    Entity.updateEntity id update layer.entities []
            in
            case maybeA of
                Nothing ->
                    updateEntity id
                        update
                        rest
                        (newLayers ++ [ layer ])

                Just newA ->
                    ( newLayers
                        ++ [ { layer
                                | entities = newEntities
                             }
                           ]
                        ++ rest
                    , Just newA
                    )

        [] ->
            ( newLayers, Nothing )
