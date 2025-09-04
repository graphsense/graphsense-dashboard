module Update.Pathfinder.Tx exposing (setAddressInTx, setFromAddress, setIoAddress, setToAddress, unsetAccountAddress, unsetAddress, updateAccount, updateAccountAddress, updateAddress, updateAddressInTx, updateIoAddress, updateUtxo, updateUtxoIo)

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
        |> Dict.update addressId (Maybe.map upd)
        |> flip set t


updateAddress : (Address -> Address) -> Io -> Io
updateAddress update io =
    { io | address = Maybe.map update io.address }


setIoAddress : Address -> Io -> Io
setIoAddress address io =
    { io
        | address = Just address
    }


updateIoAddress : (Address -> Address) -> Io -> Io
updateIoAddress upd io =
    { io
        | address = Maybe.map upd io.address
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


updateAccountAddress : Direction -> Id -> (Address -> Address) -> AccountTx -> AccountTx
updateAccountAddress _ id upd tx =
    -- ignore Direction has problems with self loops of zero value
    -- we always both addresses if the addresses match
    (if tx.to == id then
        tx.toAddress |> Maybe.map upd |> flip s_toAddress tx

     else
        tx
    )
        |> (\nexTx ->
                if nexTx.from == id then
                    nexTx.fromAddress |> Maybe.map upd |> flip s_fromAddress nexTx

                else
                    nexTx
           )


setAddressInTx : Direction -> Address -> Tx -> Tx
setAddressInTx dir a t =
    case t.type_ of
        Utxo _ ->
            setIoAddress a
                |> updateUtxoIo dir a.id
                |> flip updateUtxo t

        Account { to, from } ->
            -- ignore direction and only look at address
            -- direction only setter had problems with selfloops (esp. with null value, where its not clear if in or out)
            (if to == a.id then
                setToAddress a

             else
                identity
            )
                >> (if from == a.id then
                        setFromAddress a

                    else
                        identity
                   )
                |> flip updateAccount t


updateAddressInTx : Direction -> Id -> (Address -> Address) -> Tx -> Tx
updateAddressInTx dir id upd t =
    case t.type_ of
        Utxo _ ->
            updateIoAddress upd
                |> updateUtxoIo dir id
                |> flip updateUtxo t

        Account _ ->
            updateAccountAddress dir id upd
                |> flip updateAccount t
