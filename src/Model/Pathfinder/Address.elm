module Model.Pathfinder.Address exposing (..)

import Animation exposing (Animation, Clock)
import Api.Data exposing (Values)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Time exposing (Posix)
import Util.Data exposing (timestampToPosix)
import Hex

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
    RemoteData.unwrap Nothing (.inDegree >> Just) a.data


getOutDegree : Address -> Maybe Int
getOutDegree a =
    RemoteData.unwrap Nothing (.outDegree >> Just) a.data


getBalance : Address -> Maybe Values
getBalance a =
    RemoteData.unwrap Nothing (.balance >> Just) a.data


getTotalReceived : Address -> Maybe Values
getTotalReceived a =
    RemoteData.unwrap Nothing (.totalReceived >> Just) a.data


getCurrency : Address -> Maybe String
getCurrency a =
    RemoteData.unwrap Nothing (.currency >> Just) a.data


getActivityRange : Api.Data.Address -> ( Posix, Posix )
getActivityRange x =
    ( timestampToPosix x.firstTx.timestamp
    , timestampToPosix x.lastTx.timestamp
    )

getClusterId: Address -> Maybe String
getClusterId a = RemoteData.unwrap Nothing (.entity >> Hex.toString  >> Just) a.data