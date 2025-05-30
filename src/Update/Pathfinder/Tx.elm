module Update.Pathfinder.Tx exposing (setFromAddress, setIoAddress, setToAddress, unsetAccountAddress, unsetAddress, updateAccount, updateAddress, updateUtxo, updateUtxoIo)

import Basics.Extra exposing (flip)
import Dict
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Tx exposing (AccountTx, Io, Tx, TxType(..), UtxoTx, getAccountTx, getUtxoTx)
import RecordSetter exposing (s_address, s_fromAddress, s_inputs, s_outputs, s_toAddress, s_type_)


updateUtxo : (UtxoTx -> UtxoTx) -> Tx -> Tx
updateUtxo upd tx =
    getUtxoTx tx
        |> Maybe.map
            (upd >> Utxo >> flip s_type_ tx)
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


setIoAddress : Address -> Io -> Io
setIoAddress address io =
    { io
        | address = Just address
    }


setFromAddress : Address -> AccountTx -> AccountTx
setFromAddress address tx =
    { tx
        | fromAddress = Just address
    }


setToAddress : Address -> AccountTx -> AccountTx
setToAddress address tx =
    { tx
        | toAddress = Just address
    }


unsetAddress : Io -> Io
unsetAddress =
    s_address Nothing


updateAccount : (AccountTx -> AccountTx) -> Tx -> Tx
updateAccount upd tx =
    getAccountTx tx
        |> Maybe.map (upd >> Account >> flip s_type_ tx)
        |> Maybe.withDefault tx


unsetAccountAddress : Direction -> AccountTx -> AccountTx
unsetAccountAddress dir =
    (case dir of
        Outgoing ->
            s_toAddress

        Incoming ->
            s_fromAddress
    )
        Nothing
