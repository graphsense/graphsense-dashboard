module Model.Pathfinder.Network exposing (..)

import Animation
import Dict exposing (Dict)
import Hex
import Init.Pathfinder.Id as Id
import List.Extra
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords
import Model.Pathfinder.Address exposing (Address, txsGetSet)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import RemoteData
import Set exposing (Set)


type alias Network =
    { addresses : Dict Id Address
    , txs : Dict Id Tx
    , animatedAddresses : Set Id
    , animatedTxs : Set Id
    }


getBoundingBox : Network -> Coords.BBox
getBoundingBox net =
    let
        extractCord ( _, a ) =
            { x = a.x + a.dx, y = (a.y |> Animation.getTo) + a.dy }

        addressPos =
            Dict.toList net.addresses |> List.map extractCord

        txPos =
            Dict.toList net.txs |> List.map extractCord

        xs =
            (addressPos ++ txPos) |> List.map .x

        ys =
            (addressPos ++ txPos) |> List.map .y

        mxx =
            xs |> List.maximum |> Maybe.withDefault 0

        mxy =
            ys |> List.maximum |> Maybe.withDefault 0

        mix =
            xs |> List.minimum |> Maybe.withDefault 0

        miy =
            ys |> List.minimum |> Maybe.withDefault 0
    in
    { x = mix, y = miy, width = abs (mxx - mix), height = abs (mxy - miy) }


hasTx : Id -> Network -> Bool
hasTx id network =
    Dict.member id network.txs


hasAddress : Id -> Network -> Bool
hasAddress id network =
    Dict.member id network.addresses


getClustersOnGraph : Network -> Set Id
getClustersOnGraph net =
    net.addresses |> Dict.values |> List.filterMap (.data >> RemoteData.toMaybe) |> List.map (\x -> Id.initClusterId x.currency x.entity) |> Set.fromList


isClusterFriendAlreadyOnGraph : Id -> Network -> Bool
isClusterFriendAlreadyOnGraph id net =
    let
        clusters =
            getClustersOnGraph net
    in
    Set.member id clusters


listTxsForAddress : Network -> Id -> List ( Direction, Tx )
listTxsForAddress network id =
    network.txs
        |> Dict.values
        |> List.filterMap
            (\tx ->
                if Tx.isInFlow id tx then
                    Just ( Incoming, tx )
                    -- TODO: Revise for UTXO, depends on total flow not only if address is on the in side.

                else if Tx.isOutFlow id tx then
                    Just ( Outgoing, tx )

                else
                    Nothing
            )


hasAnimations : Network -> Bool
hasAnimations network =
    Set.isEmpty network.animatedTxs
        && Set.isEmpty network.animatedAddresses
        |> not


getRecentTxForAddress : Network -> Direction -> Id -> Maybe Tx
getRecentTxForAddress network direction addressId =
    let
        getTxSet =
            case direction of
                Incoming ->
                    .incomingTxs
                        >> txsGetSet

                Outgoing ->
                    .outgoingTxs
                        >> txsGetSet
    in
    Dict.get addressId network.addresses
        |> Maybe.andThen getTxSet
        |> Maybe.andThen
            (Set.toList
                >> List.filterMap (\txId -> Dict.get txId network.txs)
                >> List.sortBy Tx.getRawTimestamp
                >> List.Extra.last
            )


isEmpty : Network -> Bool
isEmpty { addresses, txs } =
    Dict.isEmpty addresses && Dict.isEmpty txs
