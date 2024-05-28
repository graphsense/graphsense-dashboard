module Model.Pathfinder.Address exposing (..)

import Api.Data
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Time exposing (Posix)


type alias Address =
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
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


getActivityRange : Address -> Maybe ( Posix, Posix )
getActivityRange a =
    case a.data of
        Success x ->
            Just ( Time.millisToPosix (x.firstTx.timestamp * 1000), Time.millisToPosix (x.lastTx.timestamp * 1000) )

        _ ->
            Nothing
