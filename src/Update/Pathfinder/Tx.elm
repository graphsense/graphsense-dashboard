module Update.Pathfinder.Tx exposing (updateUtxo, updateUtxoIo)

import Basics.Extra exposing (flip)
import Dict.Nonempty as NDict
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (Io, Tx, TxType(..), UtxoTx, getUtxoTx)
import RecordSetter exposing (s_inputs, s_outputs)


updateUtxo : (UtxoTx -> UtxoTx) -> Tx -> Tx
updateUtxo upd tx =
    getUtxoTx tx
        |> Maybe.map
            (\t ->
                { tx
                    | type_ = upd t |> Utxo
                }
            )
        |> Maybe.withDefault tx


updateUtxoIo : Direction -> Id -> (Io -> Io) -> UtxoTx -> UtxoTx
updateUtxoIo dir addressId upd t =
    let
        ( ios, set ) =
            case dir of
                Incoming ->
                    ( t.inputs, s_inputs )

                Outgoing ->
                    ( t.outputs, s_outputs )
    in
    ios
        |> NDict.get addressId
        |> Maybe.map
            (upd
                >> flip (NDict.insert addressId) ios
            )
        |> Maybe.withDefault ios
        |> flip set t