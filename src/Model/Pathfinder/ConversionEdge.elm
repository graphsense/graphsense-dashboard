module Model.Pathfinder.ConversionEdge exposing
    ( ConversionEdge
    , LegAmount(..)
    , LegTxs
    , drawsFromRow
    , findLegTx
    , getInputTransferId
    , getOutputTransferId
    , inputAmount
    , inputValues
    , isLegTx
    , isRowOnly
    , known
    , legTxHash
    , namedSourceChain
    , nativeAsset
    , outputAmount
    , outputValues
    , payeeId
    , payerId
    , rowEdgeId
    , toIdString
    )

import Api.Data
import Dict exposing (Dict)
import Init.Pathfinder.Id as Id
import Model.Currency as Currency exposing (AssetIdentifier)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx as Tx exposing (Tx)
import Util exposing (removeLeading0x)
import Util.Data as Data


type alias ConversionEdge =
    { id : ( Id, Id )
    , outputAddressId : Id
    , inputAddressId : Id
    , fromAsset : String
    , toAsset : String
    , inputAddress : Maybe Address
    , outputAddress : Maybe Address
    , legTxs : Maybe LegTxs
    , raw : Api.Data.ExternalConversion
    , selected : Bool
    , hovered : Bool

    -- drag offset of the swap icon from its layout position (graph units); Nothing = unmoved
    , nodeOffset : Maybe { x : Float, y : Float }
    }


{-| The two loaded leg txs an edge was drawn with, or Nothing for an edge
drawn from its conversion row alone (`drawsFromRow`).
-}
type alias LegTxs =
    { input : Api.Data.Tx
    , output : Api.Data.Tx
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
    case conversion.legTxs of
        Just _ ->
            conversion.raw.fromAssetTransfer ++ "_" ++ conversion.raw.toAssetTransfer

        Nothing ->
            -- several rows may share both transfers only if they are different
            -- transfers of one protocol, which the transfer id tells apart
            Id.id (Tuple.second conversion.id)


{-| The input leg's amount from the loaded leg tx, quoted with the conversion's
own fiat when sent (the leg tx may be unpriced). A leg tx not touching
`fromAddress` reads as zero with no fiat; an edge without leg txs reads as
zero (see `inputAmount`).
-}
inputValues : ConversionEdge -> Api.Data.Values
inputValues c =
    c.legTxs
        |> Maybe.andThen (.input >> Tx.getInputValueForAddressFromRawTx c.raw.fromAddress)
        |> legValues c.raw.fromAmountFiatValues


{-| The same for the output leg.
-}
outputValues : ConversionEdge -> Api.Data.Values
outputValues c =
    c.legTxs
        |> Maybe.andThen (.output >> Tx.getOutputValueForAddressFromRawTx c.raw.toAddress)
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


{-| Is this a bridge row the graph draws from the row alone?

A complete `bridge_tx` row (both ends known) whose token is not in the token
registry on at least one side. Such a row proves the bridge, but the backends
serve transfers of registry tokens only, so its `_T<n>` leg is no tx the graph
could load (DASHBOARD\_CHANGES D-30). The edge is drawn from the row itself,
between its payer and payee. Swaps, half rows (an end `unknown`, a transfer
`pending`) and rows whose tokens are all registered are not.

-}
drawsFromRow : Api.Data.ExternalConversion -> Bool
drawsFromRow c =
    (c.conversionType == Api.Data.ExternalConversionConversionTypeBridgeTx)
        && not (c.fromIsSupportedAsset && c.toIsSupportedAsset)
        && List.all known
            [ c.fromNetwork
            , c.toNetwork
            , c.fromAddress
            , c.toAddress
            , c.fromAsset
            , c.toAsset
            , c.fromAssetTransfer
            , c.toAssetTransfer
            ]


{-| The markers a half row puts on its missing side.
-}
known : String -> Bool
known value =
    not (List.member (String.toLower (String.trim value)) [ "", "unknown", "pending" ])


{-| The chain a half row names for the source leg it does not have (a fill
on this network of a deposit on Linea or Solana): the backend names such a
chain in `from_network` although it serves nothing of it.
-}
namedSourceChain : Api.Data.ExternalConversion -> Maybe String
namedSourceChain c =
    if not (known c.fromAssetTransfer) && known c.fromNetwork then
        Just c.fromNetwork

    else
        Nothing


{-| An edge drawn from its row (`drawsFromRow`), not from loaded leg txs.
-}
isRowOnly : ConversionEdge -> Bool
isRowOnly =
    .legTxs >> (==) Nothing


{-| The key of an edge drawn from its row: the row's own identity, its source
and destination transfers plus protocol and transfer id, so every row is an
edge of its own, also when several rows join the same txs. Prefixed so it can
never equal the tx ids an edge between loaded legs is keyed by.
-}
rowEdgeId : Api.Data.ExternalConversion -> ( Id, Id )
rowEdgeId c =
    let
        transfer =
            removeLeading0x >> String.toLower
    in
    ( Id.init c.fromNetwork ("row:" ++ transfer c.fromAssetTransfer)
    , Id.init c.toNetwork
        ("row:"
            ++ transfer c.toAssetTransfer
            ++ "|"
            ++ Maybe.withDefault "" c.protocol
            ++ "|"
            ++ Maybe.withDefault "" c.transferId
        )
    )


{-| The row's payer, on the source network.
-}
payerId : Api.Data.ExternalConversion -> Id
payerId c =
    Id.init c.fromNetwork (Data.normalizeIdentifier c.fromNetwork c.fromAddress)


{-| The row's payee, on the destination network.
-}
payeeId : Api.Data.ExternalConversion -> Id
payeeId c =
    Id.init c.toNetwork (Data.normalizeIdentifier c.toNetwork c.toAddress)


{-| The tx hash of a leg as the row names it (`<hash>_T<n>`, `<hash>_I<n>` or
the bare hash), lowercased and without `0x`.
-}
legTxHash : String -> String
legTxHash transfer =
    removeLeading0x transfer
        |> String.split "_"
        |> List.head
        |> Maybe.withDefault transfer
        |> String.toLower


{-| What a leg's amount can honestly be shown as.
-}
type LegAmount
    = Amount AssetIdentifier Api.Data.Values
      -- the leg's token is not in the registry: no decimals, so no amount
    | NotInRegistry


{-| The input leg's amount: from the loaded leg tx as before (a native leg from
the row, `nativeLegValues`), or the row's own amount (when its token is
registered) for an edge drawn from its row, and for a leg tx that does not
touch the row's `fromAddress` at all: a swap paid through an intermediary
names the Transfer the intermediary made, not one by the trader
(DASHBOARD\_CHANGES D-41, pol 0x1733adc7).
-}
inputAmount : ConversionEdge -> LegAmount
inputAmount c =
    case c.legTxs of
        Just legs ->
            if Tx.getInputValueForAddressFromRawTx c.raw.fromAddress legs.input == Nothing && c.raw.fromAsset /= nativeAsset then
                rowAmount c.raw.fromIsSupportedAsset c.raw.fromNetwork c.raw.fromAsset c.raw.fromAmount c.raw.fromAmountFiatValues

            else
                Amount (Currency.asset c.raw.fromNetwork (Tx.getAssetFromRawTx legs.input))
                    (nativeLegValues c.raw.fromAsset c.raw.fromAmount c.raw.fromAmountFiatValues (inputValues c))

        Nothing ->
            rowAmount c.raw.fromIsSupportedAsset c.raw.fromNetwork c.raw.fromAsset c.raw.fromAmount c.raw.fromAmountFiatValues


{-| The same for the output leg, on the destination network: a leg tx that does
not pay the row's `toAddress` (the swap's output Transfer paid the
intermediary) shows the row's amount.
-}
outputAmount : ConversionEdge -> LegAmount
outputAmount c =
    case c.legTxs of
        Just legs ->
            if Tx.getOutputValueForAddressFromRawTx c.raw.toAddress legs.output == Nothing && c.raw.toAsset /= nativeAsset then
                rowAmount c.raw.toIsSupportedAsset c.raw.toNetwork c.raw.toAsset c.raw.toAmount c.raw.toAmountFiatValues

            else
                Amount (Currency.asset c.raw.toNetwork (Tx.getAssetFromRawTx legs.output))
                    (nativeLegValues c.raw.toAsset c.raw.toAmount c.raw.toAmountFiatValues (outputValues c))

        Nothing ->
            rowAmount c.raw.toIsSupportedAsset c.raw.toNetwork c.raw.toAsset c.raw.toAmount c.raw.toAmountFiatValues


{-| A native leg is the whole tx (DASHBOARD\_CHANGES D-37), whose own value is
the leg's amount only when the payer calls the bridge directly: an Across fill
pays the recipient through an internal transfer, so the fill tx moves 0 to
them. The row's amount, then, with the leg tx's fiat where the tx moved that
amount. Otherwise the row's quote, and without one (the backend often sends
none) the leg tx's fiat scaled to the row's amount: the same tx, so the same
rate. A fill tx that moved nothing has no fiat to scale and shows none.
-}
nativeLegValues : String -> String -> Maybe (List Api.Data.Rate) -> Api.Data.Values -> Api.Data.Values
nativeLegValues asset amount rates legTxValues =
    case ( asset == nativeAsset, hexAmount amount ) of
        ( True, Just value ) ->
            if sameAmount legTxValues.value value then
                legTxValues

            else
                case rates of
                    Just ((_ :: _) as quote) ->
                        { value = value, fiatValues = quote }

                    _ ->
                        { value = value, fiatValues = scaledFiat value legTxValues }

        _ ->
            legTxValues


{-| Equal amounts, allowing for the rounding above 2^53 base units (about
0.009 ETH): `hexAmount` and the JSON number can then land a unit in the last
place apart. Below that, only equal values are the same.
-}
sameAmount : Int -> Int -> Bool
sameAmount a b =
    toFloat (abs (a - b)) <= toFloat (max (abs a) (abs b)) * 4.5e-16


{-| The leg tx's fiat values for `value` base units instead of its own value.
-}
scaledFiat : Int -> Api.Data.Values -> List Api.Data.Rate
scaledFiat value legTxValues =
    if legTxValues.value == 0 then
        []

    else
        let
            factor =
                toFloat value / toFloat legTxValues.value
        in
        legTxValues.fiatValues
            |> List.map (\r -> { r | value = r.value * factor })


{-| A registered token is named by its contract, which the formatter resolves
to the registry's ticker and decimals; the network's coin by the network.
-}
rowAmount : Bool -> String -> String -> String -> Maybe (List Api.Data.Rate) -> LegAmount
rowAmount supported network asset amount rates =
    let
        assetId =
            if asset == nativeAsset then
                Currency.assetFromBase network

            else
                Currency.asset network (String.toLower asset)
    in
    case ( supported, hexAmount amount ) of
        ( True, Just value ) ->
            Amount assetId { value = value, fiatValues = Maybe.withDefault [] rates }

        _ ->
            NotInRegistry


{-| A row's hex amount in base units. An Int is a double at runtime, so a
uint256 beyond 2^53 loses its last digits, below what the formatter shows.
-}
hexAmount : String -> Maybe Int
hexAmount amount =
    let
        digits =
            String.toLower amount |> removeLeading0x
    in
    if String.isEmpty digits then
        Nothing

    else
        String.foldl
            (\ch acc ->
                Maybe.map2 (\a d -> a * 16 + d) acc (hexDigit ch)
            )
            (Just 0)
            digits


hexDigit : Char -> Maybe Int
hexDigit ch =
    let
        code =
            Char.toCode ch
    in
    if code >= 48 && code <= 57 then
        Just (code - 48)

    else if code >= 97 && code <= 102 then
        Just (code - 87)

    else
        Nothing
