module Model.Pathfinder.Network exposing (..)

import Dict exposing (Dict)
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Set exposing (Set)


type alias Network =
    { addresses : Dict Id Address
    , txs : Dict Id Tx
    , animatedAddresses : Set Id
    , animatedTxs : Set Id
    }


hasTx : Id -> Network -> Bool
hasTx id network =
    Dict.member id network.txs


hasAddress : Id -> Network -> Bool
hasAddress id network =
    Dict.member id network.addresses


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
