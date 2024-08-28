module Model.Pathfinder.Network exposing (..)

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



-- getBoundingBox : Network -> Coords.BBox
-- getBoundingBox net =
--     let
--         addressPos = Dict.toList net.addresses |> List.map (\(id, a) -> {x = a.x + a.dx, y=a.y + a.dy })
--      in
--     { x = 0.0, y = 0.0, width = 0.0, height = 0.0 }


hasTx : Id -> Network -> Bool
hasTx id network =
    Dict.member id network.txs


hasAddress : Id -> Network -> Bool
hasAddress id network =
    Dict.member id network.addresses


getClustersOnGraph : Network -> Set Id
getClustersOnGraph net =
    net.addresses |> Dict.values |> List.filterMap (.data >> RemoteData.toMaybe) |> List.map (\x -> Id.init x.currency (Hex.toString x.entity)) |> Set.fromList


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
                if Tx.hasInput id tx then
                    Just ( Incoming, tx )

                else if Tx.hasOutput id tx then
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
