module Model.Pathfinder.Address exposing
    ( Address
    , AddressServiceType(..)
    , Txs(..)
    , expandAllowed
    , getActivityRange
    , getActivityRangeAddress
    , getAddressType
    , getBalance
    , getClusterId
    , getCoords
    , getTotalReceived
    , getTotalSpent
    , getTxs
    , isSharedService
    , isSmartContract
    , txsGetSet
    , txsSetter
    , txsToSet
    )

import Animation exposing (Animation, Clock)
import Api.Data exposing (Values)
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import Maybe.Extra
import Model.Direction exposing (Direction(..))
import Model.Entity exposing (isPossibleServiceUtxo)
import Model.Graph.Coords exposing (Coords)
import Model.Pathfinder.Id as Id exposing (Id)
import Plugin.Model as Plugin
import RecordSetter exposing (s_incomingTxs, s_outgoingTxs)
import RemoteData exposing (WebData)
import Set exposing (Set)
import Time exposing (Posix)
import Util.Data exposing (isAccountLike, timestampToPosix)


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
    , clusterSiblingHovered : Bool
    , exchange : Maybe String
    , hasTags : Bool
    , hasClusterTagsOnly : Bool
    , networks : Dict String (Set String)
    , actor : Maybe String
    , isStartingPoint : Bool
    , plugins : Plugin.AddressState
    , clusterColor : Maybe String
    , addressServiceType : AddressServiceType
    }


type Txs
    = Txs (Set Id)
    | TxsLastCheckedChangeTx Api.Data.TxUtxo
    | TxsLoading
    | TxsNotFetched


type AddressServiceType
    = KnownService
    | LikelyUnknownService
    | UnknownService


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


getCoords : Address -> Coords
getCoords a =
    Coords (a.x + a.dx) (Animation.animate a.clock a.y + a.dy)


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


{-| Whether the address belongs to a shared service — infrastructure used by
many unrelated parties (exchange or smart contract), so tags or case
connections on it are weak evidence of linkage. The `addressServiceType`
heuristics are deliberately not considered here: they would flag ordinary
addresses (e.g. any UTXO address in a larger cluster).
-}
isSharedService : Address -> Bool
isSharedService address =
    (address.exchange /= Nothing)
        || isSmartContract address


expandAllowed : Address -> Bool
expandAllowed address =
    address.exchange == Nothing && (address |> isSmartContract |> not)


getClusterId : Address -> Maybe Id
getClusterId { data } =
    data
        |> RemoteData.toMaybe
        |> Maybe.map Id.initClusterIdFromAddress


{-| The server-side `is_possible_service` verdict, when present, replaces the
structural judgment (cluster shape / degree thresholds); the actor still
decides known vs. unknown. Absent (old server) = local heuristics.

REMOVABLE: once every deployed backend serves `is_possible_service` on
address detail (graphsense-lib >= the external-backend-capabilities release
computes it for account AND utxo networks), the `Nothing` branch below,
`isPossibleServiceAccountLike`, and `Model.Entity.isPossibleServiceUtxo` can
be deleted — but only after checking that no code path feeds this function
an embedded listing row (those carry no `is_possible_service`).

-}
getAddressType : Address -> Maybe Api.Data.Cluster -> AddressServiceType
getAddressType address cluster =
    case address.data |> RemoteData.toMaybe |> Maybe.andThen .isPossibleService of
        Just True ->
            if address.actor == Nothing then
                LikelyUnknownService

            else
                KnownService

        Just False ->
            if (address.id |> Id.network |> isAccountLike) && (address.actor |> Maybe.Extra.isJust) then
                KnownService

            else
                UnknownService

        Nothing ->
            if Maybe.map isPossibleServiceUtxo cluster |> Maybe.withDefault False then
                if address.actor == Nothing then
                    LikelyUnknownService

                else
                    KnownService

            else if (address.id |> Id.network |> isAccountLike) && (address.actor |> Maybe.Extra.isJust) then
                KnownService

            else if (address.id |> Id.network |> isAccountLike) && isPossibleServiceAccountLike address then
                LikelyUnknownService

            else
                UnknownService


isPossibleServiceAccountLike : Address -> Bool
isPossibleServiceAccountLike address =
    address.data
        |> RemoteData.toMaybe
        |> Maybe.map
            (\apiAddress ->
                let
                    maxDegree =
                        7500

                    maxTxs =
                        500
                in
                apiAddress.inDegree > maxDegree || apiAddress.noIncomingTxs > maxTxs
            )
        |> Maybe.withDefault False
