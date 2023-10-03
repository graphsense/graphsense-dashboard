module Update.Graph.Link exposing (..)

import Model.Graph.Layer exposing (Layer)
import Model.Graph.Link exposing (..)


entityNeighborsToLinks : IntDict Layer -> List Api.Data.EntityNeighbor -> List ( EntityId, Link Entity )
entityNeighborsToLinks layers neighbors =
    List.foldl
        (\_ acc ->
            Layer.getEntity
                Dict.foldl
                (\_ entity acc_ ->
                    if entity.entity.currency == currency && entity.entity.entity == id then
                        entity :: acc_

                    else
                        acc_
                )
                acc
                layer.entities
        )
        []
        neighbors
