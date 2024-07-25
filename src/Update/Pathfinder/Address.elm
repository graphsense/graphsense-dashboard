module Update.Pathfinder.Address exposing (..)

import Model.Pathfinder.Address exposing (..)
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
            map set
                |> Txs

        _ ->
            txs


txsInsertId : Id -> Txs -> Txs
txsInsertId id txs =
    txsToSet txs
        |> Set.insert id
        |> Txs
