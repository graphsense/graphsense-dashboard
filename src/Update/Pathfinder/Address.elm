module Update.Pathfinder.Address exposing (invalidatePrefetched, removeTx, txsInsertId)

import Model.Pathfinder.Address exposing (Address, Txs(..), txsToSet)
import Model.Pathfinder.Id exposing (Id)
import Set exposing (Set)


removeTx : Id -> Address -> Address
removeTx id address =
    { address
        | incomingTxs = txsSetMap (Set.remove id) address.incomingTxs
        , outgoingTxs = txsSetMap (Set.remove id) address.outgoingTxs
    }


txsSetMap : (Set Id -> Set Id) -> Txs -> Txs
txsSetMap map txs =
    case txs of
        Txs set ->
            let
                newSet =
                    map set
            in
            if Set.isEmpty newSet then
                TxsNotFetched

            else
                Txs newSet

        _ ->
            txs


txsInsertId : Id -> Txs -> Txs
txsInsertId id txs =
    txsToSet txs
        |> Set.insert id
        |> Txs


{-| Drop a parked next-tx prefetch. The live expand workflow anchors on the
most recent adjacent tx on the graph in the OPPOSITE direction — so whenever a
tx lands next to an address, the other direction's parked answer may be based
on an outdated anchor. Resetting to TxsNotFetched makes the next expand click
run the live lookup again (exactly the pre-prefetch behavior).
-}
invalidatePrefetched : Txs -> Txs
invalidatePrefetched txs =
    case txs of
        TxsPrefetched _ ->
            TxsNotFetched

        _ ->
            txs
