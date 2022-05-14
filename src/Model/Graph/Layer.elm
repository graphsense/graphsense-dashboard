module Model.Graph.Layer exposing (..)

import Dict exposing (Dict)
import IntDict exposing (IntDict)
import List.Extra
import Model.Graph.Address exposing (..)
import Model.Graph.Entity exposing (..)
import Model.Graph.Id as Id exposing (..)


type alias Layer =
    { id : Int
    , entities : Dict EntityId Entity
    , x : Float
    }


addresses : IntDict Layer -> List Address
addresses =
    IntDict.foldl
        (\_ layer addrs ->
            Dict.foldl
                (\_ entity addrs_ ->
                    addrs_ ++ Dict.values entity.addresses
                )
                addrs
                layer.entities
        )
        []


entities : IntDict Layer -> List Entity
entities =
    IntDict.foldl
        (\_ layer ents ->
            ents ++ Dict.values layer.entities
        )
        []


getEntity : EntityId -> IntDict Layer -> Maybe Entity
getEntity id =
    IntDict.get (Id.layer id)
        >> Maybe.andThen (.entities >> Dict.get id)


getEntities : { currency : String, entity : Int } -> IntDict Layer -> List Entity
getEntities { currency, entity } =
    IntDict.foldl
        (\_ layer acc ->
            layer.entities
                |> Dict.foldl
                    (\_ entityNode acc_ ->
                        if currency == entityNode.entity.currency && entity == entityNode.entity.entity then
                            entityNode :: acc_

                        else
                            acc_
                    )
                    acc
        )
        []
