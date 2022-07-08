module Model.Graph.Layer exposing (..)

import Api.Data
import Config.Graph exposing (entityWidth, expandHandleWidth)
import Dict exposing (Dict)
import Init.Graph.Id as Id exposing (..)
import IntDict exposing (IntDict)
import List.Extra
import Maybe.Extra
import Model.Address as A
import Model.Graph.Address as Address exposing (..)
import Model.Graph.Entity as Entity exposing (..)
import Model.Graph.Id as Id exposing (..)
import Model.Graph.Link as Link exposing (..)
import Model.Graph.Transform as Transform
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


getEntityForAddress : A.Address -> IntDict Layer -> Maybe Api.Data.Entity
getEntityForAddress address layers =
    layers
        |> IntDict.values
        |> getEntityForAddressHelp address


getEntityForAddressHelp : A.Address -> List Layer -> Maybe Api.Data.Entity
getEntityForAddressHelp address layers =
    case layers of
        [] ->
            Nothing

        layer :: rest ->
            case
                layer.entities
                    |> Dict.values
                    |> getEntityForAddressHelp2 address
            of
                Nothing ->
                    getEntityForAddressHelp address rest

                Just found ->
                    Just found


getEntityForAddressHelp2 : A.Address -> List Entity -> Maybe Api.Data.Entity
getEntityForAddressHelp2 { currency, address } =
    List.Extra.find
        (.addresses
            >> Dict.values
            >> List.Extra.find
                (\a ->
                    a.address.currency
                        == currency
                        && a.address.address
                        == address
                )
            >> (/=) Nothing
        )
        >> Maybe.map .entity


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


getAddressLinksByTarget : AddressId -> Layer -> List ( Address, Link Address )
getAddressLinksByTarget tgt layer =
    layer.entities
        |> Dict.foldl
            (\_ entity found ->
                entity.addresses
                    |> Dict.foldl
                        (\_ address found_ ->
                            case address.links of
                                Address.Links links ->
                                    Dict.get tgt links
                                        |> Maybe.map (\link -> ( address, link ) :: found_)
                                        |> Maybe.withDefault found_
                        )
                        found
            )
            []


getEntityLinksByTarget : EntityId -> Layer -> List ( Entity, Link Entity )
getEntityLinksByTarget tgt layer =
    layer.entities
        |> Dict.foldl
            (\_ entity found_ ->
                case entity.links of
                    Entity.Links links ->
                        Dict.get tgt links
                            |> Maybe.map (\link -> ( entity, link ) :: found_)
                            |> Maybe.withDefault found_
            )
            []


getFirstAddress : { currency : String, address : String } -> IntDict Layer -> Maybe Address
getFirstAddress { currency, address } layers =
    layers
        |> IntDict.foldl
            (\layerId _ found ->
                case found of
                    Just _ ->
                        found

                    Nothing ->
                        getAddress (Id.initAddressId { currency = currency, id = address, layer = layerId }) layers
            )
            Nothing


getFirstEntity : { currency : String, entity : Int } -> IntDict Layer -> Maybe Entity
getFirstEntity { currency, entity } layers =
    layers
        |> IntDict.foldl
            (\layerId _ found ->
                case found of
                    Just _ ->
                        found

                    Nothing ->
                        getEntity (Id.initEntityId { currency = currency, id = entity, layer = layerId }) layers
            )
            Nothing


getBoundingBox : IntDict Layer -> Maybe Transform.BBox
getBoundingBox layers =
    let
        getTopMost =
            Dict.foldl
                (\_ entity topMost ->
                    case topMost of
                        Nothing ->
                            Just entity

                        Just tm ->
                            if Entity.getY tm > Entity.getY entity then
                                Just entity

                            else
                                Just tm
                )
                Nothing

        getBottomMost =
            Dict.foldl
                (\_ entity topMost ->
                    case topMost of
                        Nothing ->
                            Just entity

                        Just tm ->
                            if Entity.getY tm + Entity.getHeight tm > Entity.getY entity + Entity.getHeight entity then
                                Just entity

                            else
                                Just tm
                )
                Nothing
    in
    Maybe.Extra.andThen2
        (\( _, fst ) ( _, lst ) ->
            Maybe.map2
                (\upperLeft lowerRight ->
                    { x = Entity.getX upperLeft
                    , y = Entity.getY upperLeft
                    , width = Entity.getX upperLeft + Entity.getX lowerRight + Entity.getWidth lowerRight
                    , height = Entity.getY upperLeft + Entity.getY lowerRight + Entity.getHeight lowerRight
                    }
                )
                (getTopMost fst.entities)
                (getBottomMost lst.entities)
        )
        (IntDict.findMin layers)
        (IntDict.findMax layers)
