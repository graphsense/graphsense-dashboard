module Model.Graph.Layer exposing (..)

import Config.Graph exposing (entityWidth, expandHandleWidth)
import Dict exposing (Dict)
import IntDict exposing (IntDict)
import List.Extra
import Model.Graph.Address as Address exposing (..)
import Model.Graph.Entity as Entity exposing (..)
import Model.Graph.Id as Id exposing (..)
import Model.Graph.Link as Link exposing (..)
import Tuple exposing (..)


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


getAddress : AddressId -> IntDict Layer -> Maybe Address
getAddress id =
    IntDict.get (Id.layer id)
        >> Maybe.andThen
            (.entities
                >> Dict.foldl
                    (\_ entity found ->
                        case found of
                            Nothing ->
                                Dict.get id entity.addresses

                            Just f ->
                                Just f
                    )
                    Nothing
            )


getEntities : String -> Int -> IntDict Layer -> List Entity
getEntities currency entity =
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


getAddresses : { currency : String, address : String } -> IntDict Layer -> List Address
getAddresses { currency, address } =
    IntDict.foldl
        (\_ layer acc ->
            layer.entities
                |> Dict.foldl
                    (\_ entityNode acc_ ->
                        entityNode.addresses
                            |> Dict.foldl
                                (\_ addressNode acc__ ->
                                    if currency == addressNode.address.currency && address == addressNode.address.address then
                                        addressNode :: acc__

                                    else
                                        acc__
                                )
                                acc_
                    )
                    acc
        )
        []


getLeftBound : Layer -> Float
getLeftBound =
    getX


getX : Layer -> Float
getX layer =
    layer.entities
        |> Dict.foldl
            (\_ entity mn ->
                let
                    x =
                        Entity.getX entity
                in
                mn
                    |> Maybe.map (min x)
                    |> Maybe.withDefault x
                    |> Just
            )
            Nothing
        |> Maybe.withDefault layer.x


getRightBound : Layer -> Float
getRightBound layer =
    layer.entities
        |> Dict.foldl
            (\_ entity mn ->
                let
                    x =
                        Entity.getX entity + Entity.getWidth entity
                in
                mn
                    |> Maybe.map (min x)
                    |> Maybe.withDefault x
                    |> Just
            )
            Nothing
        |> Maybe.withDefault (layer.x + entityWidth + 2 * expandHandleWidth)


getEntityLink : LinkId EntityId -> IntDict Layer -> Maybe ( Entity, Link Entity )
getEntityLink ( src, tgt ) =
    getEntity src
        >> Maybe.andThen
            (\entity ->
                case entity.links of
                    Entity.Links links ->
                        Dict.get tgt links
                            |> Maybe.map (pair entity)
            )


getAddressLink : LinkId AddressId -> IntDict Layer -> Maybe ( Address, Link Address )
getAddressLink ( src, tgt ) =
    getAddress src
        >> Maybe.andThen
            (\address ->
                case address.links of
                    Address.Links links ->
                        Dict.get tgt links
                            |> Maybe.map (pair address)
            )
