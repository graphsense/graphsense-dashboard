module Model.Pathfinder.Address exposing (..)

import Api.Data
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)


type alias Address =
    { x : Float
    , y : Float
    , id : Id
    , incomingTxs : Set Id
    , outgoingTxs : Set Id
    , data : WebData Api.Data.Address
    , selected : Bool
    }


getNrTxs : Address -> Maybe Int
getNrTxs a =
    case a.data of
        Success x ->
            Just (x.noOutgoingTxs + x.noIncomingTxs)

        _ ->
            Nothing


getInDegree : Address -> Maybe Int
getInDegree a =
    case a.data of
        Success x ->
            Just x.inDegree

        _ ->
            Nothing


getOutDegree : Address -> Maybe Int
getOutDegree a =
    case a.data of
        Success x ->
            Just x.outDegree

        _ ->
            Nothing
