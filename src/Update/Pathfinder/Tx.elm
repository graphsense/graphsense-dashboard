module Update.Pathfinder.Tx exposing (setAddress, unsetAddress, updateAddress, updateUtxo, updateUtxoIo)

import Basics.Extra exposing (flip)
import Dict
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (AccountTx, Io, Tx, TxType(..), UtxoTx, getUtxoTx)
import RecordSetter exposing (s_address, s_inputs, s_outputs)


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
        |> Dict.get addressId
        |> Maybe.map
            (upd
                >> flip (Dict.insert addressId) ios
            )
        |> Maybe.withDefault ios
        |> flip set t


updateAddress : (Address -> Address) -> Io -> Io
updateAddress update io =
    { io | address = Maybe.map update io.address }


setAddress : Address -> Io -> Io
setAddress address io =
    { io
        | address = Just address
    }


unsetAddress : Io -> Io
unsetAddress =
    s_address Nothing
