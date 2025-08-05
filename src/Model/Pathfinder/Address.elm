module Model.Pathfinder.Address exposing
    ( Address
    , Txs(..)
    , expandAllowed
    , getActivityRange
    , getActivityRangeAddress
    , getBalance
    , getCoords
    , getInDegree
    , getNrTxs
    , getOutDegree
    , getTotalReceived
    , getTotalSpent
    , getTxs
    , txsGetSet
    , txsSetter
    , txsToSet
    )

import Animation exposing (Animation, Clock)
import Api.Data exposing (Values)
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id exposing (Id)
import Plugin.Model as Plugin
import RecordSetter exposing (s_incomingTxs, s_outgoingTxs)
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
    , incomingTxs : Txs
    , outgoingTxs : Txs
    , data : WebData Api.Data.Address
    , selected : Bool
    , exchange : Maybe String
    , hasTags : Bool
    , actor : Maybe String
    , isStartingPoint : Bool
    , plugins : Plugin.AddressState
    }


type Txs
    = Txs (Set Id)
    | TxsLastCheckedChangeTx Api.Data.TxUtxo
    | TxsLoading
    | TxsNotFetched


txsGetSet : Txs -> Maybe (Set Id)
txsGetSet txs =
    case txs of
        Txs set ->
            Just set

        _ ->
            Nothing


txsToSet : Txs -> Set Id
txsToSet txs =
    case txs of
        Txs set ->
            set

        _ ->
            Set.empty


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


getTotalSpent : Address -> Maybe Values
getTotalSpent a =
    RemoteData.unwrap Nothing (.totalSpent >> Just) a.data


isSmartContract : Address -> Bool
isSmartContract a =
    RemoteData.unwrap Nothing .isContract a.data |> Maybe.withDefault False


getActivityRangeAddress : Address -> Maybe ( Posix, Posix )
getActivityRangeAddress a =
    RemoteData.unwrap Nothing (getActivityRange >> Just) a.data


getActivityRange : Api.Data.Address -> ( Posix, Posix )
getActivityRange x =
    ( timestampToPosix x.firstTx.timestamp
    , timestampToPosix x.lastTx.timestamp
    )


getTxs : Address -> Direction -> Txs
getTxs address direction =
    case direction of
        Incoming ->
            address.incomingTxs

        Outgoing ->
            address.outgoingTxs


txsSetter : Direction -> (Txs -> Address -> Address)
txsSetter direction =
    case direction of
        Incoming ->
            s_incomingTxs

        Outgoing ->
            s_outgoingTxs


expandAllowed : Address -> Bool
expandAllowed address =
    address.exchange == Nothing && (address |> isSmartContract |> not)
