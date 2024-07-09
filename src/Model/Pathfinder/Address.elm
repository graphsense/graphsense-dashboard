module Model.Pathfinder.Address exposing (..)

import Animation exposing (Animation, Clock)
import Api.Data exposing (Values)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Time exposing (Posix)
import Util.Data exposing (timestampToPosix)


type alias Address =
    { x : Float
    , y : Animation
    , clock : Clock
    , dx : Float
    , dy : Float
    , opacity : Animation
    , id : Id
    , incomingTxs : Set Id
    , outgoingTxs : Set Id
    , data : WebData Api.Data.Address
    , selected : Bool
    , exchange : Maybe String
    , hasTags : Bool
    , hasActor : Bool
    , isStartingPoint : Bool
    }


getNrTxs : Address -> Maybe Int
getNrTxs a =
    case a.data of
        Success x ->
            Just (x.noOutgoingTxs + x.noIncomingTxs)

        _ ->
            Nothing


getCoords : Address -> Coords
getCoords a =
    Coords (a.x + a.dx) (Animation.animate a.clock a.y + a.dy)


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


getBalance : Address -> Maybe Values
getBalance a =
    case a.data of
        Success x ->
            Just x.balance

        _ ->
            Nothing


getTotalReceived : Address -> Maybe Values
getTotalReceived a =
    case a.data of
        Success x ->
            Just x.totalReceived

        _ ->
            Nothing


getActivityRange : Api.Data.Address -> ( Posix, Posix )
getActivityRange x =
    ( timestampToPosix x.firstTx.timestamp
    , timestampToPosix x.lastTx.timestamp
    )
