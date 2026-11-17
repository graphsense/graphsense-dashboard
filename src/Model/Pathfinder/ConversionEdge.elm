module Model.Pathfinder.ConversionEdge exposing (ConversionEdge, findLegTx, getInputTransferId, getOutputTransferId, inputValues, isLegTx, nativeAsset, outputValues, toIdString)

import Api.Data
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Util exposing (removeLeading0x)


type alias ConversionEdge =
    { id : ( Id, Id )
    , outputAddressId : Id
    , inputAddressId : Id
    , fromAsset : String
    , toAsset : String
    , inputAddress : Maybe Address
    , outputAddress : Maybe Address
    , rawInputTransaction : Api.Data.Tx
    , rawOutputTransaction : Api.Data.Tx
    , raw : Api.Data.ExternalConversion
    , selected : Bool
    , hovered : Bool

    -- drag offset of the swap icon from its layout position (graph units); Nothing = unmoved
    , nodeOffset : Maybe { x : Float, y : Float }
    }


{-| The asset a conversion leg names for the network's own coin.
-}
nativeAsset : String
nativeAsset =
    "native"


{-| A leg as the conversions endpoint names it, made comparable to a tx id.

The endpoint prefixes legs with `0x`, the txs endpoint does not. A native
bridge leg is the BARE tx hash (DASHBOARD\_CHANGES D-37: there is no Transfer
to name), meaning the whole tx, and address tx lists name it the same way. The
bare hash and `<hash>_I0` are one key here; every other `_T<n>`/`_I<n>` stays
as is. `/txs/{hash}` serves the whole tx as its ROOT transfer
`<hash>_I<root trace>`, which is `_I0` only for the first trace of a block, so
an id alone cannot tell the root from any other internal transfer: that takes
the tx (`isLegTx`).

-}
legKey : String -> String
legKey transfer =
    let
        t =
            removeLeading0x transfer
    in
    if String.endsWith "_I0" t then
        String.dropRight 3 t

    else
        t


{-| Is `txid` the leg a conversion names as `transfer` on `network`, judged by
the id alone? Misses a bare-hash leg served as a root other than `_I0`.
-}
isLeg : Id -> String -> String -> Bool
isLeg txid network transfer =
    Id.network txid == network && legKey (Id.id txid) == legKey transfer


{-| Is `tx` the leg a conversion names as `transfer` on `network`? As `isLeg`,
and a bare-hash leg is also the tx's root transfer under any trace index
(`<hash>_I412` with `is_external`), but no other internal transfer of it.
-}
isLegTx : Api.Data.Tx -> String -> String -> Bool
isLegTx tx network transfer =
    isLeg (Tx.getTxId tx) network transfer || isRootTransferOf network transfer tx


{-| `tx` is the root transfer of the tx a bare-hash leg names.
-}
isRootTransferOf : String -> String -> Api.Data.Tx -> Bool
isRootTransferOf network transfer tx =
    let
        key =
            legKey transfer

        txid =
            Tx.getTxId tx
    in
    case tx of
        Api.Data.TxTxAccount t ->
            not (String.contains "_" key)
                && (t.isExternal == Just True)
                && (Id.network txid == network)
                && String.startsWith (key ++ "_I") (removeLeading0x (Id.id txid))

        Api.Data.TxTxUtxo _ ->
            False


{-| The leg's tx if the graph holds it, under whichever id it was loaded with:
the sub-transfer itself, or for the whole tx its bare hash, its `_I0`, or its
root transfer under another trace index.
-}
findLegTx : String -> String -> Dict Id Tx -> Maybe Tx
findLegTx network transfer txs =
    let
        key =
            legKey transfer

        candidates =
            if String.contains "_" key then
                [ key ]

            else
                [ key, key ++ "_I0" ]

        byId =
            candidates
                |> List.filterMap (\c -> Dict.get (Id.init network c) txs)
                |> List.head
    in
    case byId of
        Just leg ->
            Just leg

        Nothing ->
            if String.contains "_" key then
                Nothing

            else
                txs
                    |> Dict.values
                    |> List.filter (Tx.getRawTx >> isRootTransferOf network transfer)
                    |> List.head


{-| The edge's output leg, as the tx id it was drawn with.
-}
getOutputTransferId : ConversionEdge -> Id
getOutputTransferId =
    .id >> Tuple.second


{-| The edge's input leg, as the tx id it was drawn with.
-}
getInputTransferId : ConversionEdge -> Id
getInputTransferId =
    .id >> Tuple.first


toIdString : ConversionEdge -> String
toIdString conversion =
    conversion.raw.fromAssetTransfer ++ "_" ++ conversion.raw.toAssetTransfer


{-| The input leg's amount from the loaded leg tx, quoted with the conversion's
own fiat when sent (the leg tx may be unpriced). A leg tx not touching
`fromAddress` reads as zero with no fiat.
-}
inputValues : ConversionEdge -> Api.Data.Values
inputValues c =
    Tx.getInputValueForAddressFromRawTx c.raw.fromAddress c.rawInputTransaction
        |> legValues c.raw.fromAmountFiatValues


{-| The same for the output leg.
-}
outputValues : ConversionEdge -> Api.Data.Values
outputValues c =
    Tx.getOutputValueForAddressFromRawTx c.raw.toAddress c.rawOutputTransaction
        |> legValues c.raw.toAmountFiatValues


legValues : Maybe (List Api.Data.Rate) -> Maybe Api.Data.Values -> Api.Data.Values
legValues rates =
    Maybe.map (withFiat rates)
        >> Maybe.withDefault { value = 0, fiatValues = [] }


withFiat : Maybe (List Api.Data.Rate) -> Api.Data.Values -> Api.Data.Values
withFiat rates values =
    rates
        |> Maybe.map (\rs -> { values | fiatValues = rs })
        |> Maybe.withDefault values
