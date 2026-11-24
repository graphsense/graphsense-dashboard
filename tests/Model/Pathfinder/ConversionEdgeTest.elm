module Model.Pathfinder.ConversionEdgeTest exposing (suite)

import Api.Data
import Expect
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Model.Currency as Currency
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Support.SwapFixture exposing (accountTx, dexSwap, inputLegId, outputLegId, settlement, swapper, usd1Contract)
import Test exposing (Test, describe, test)


stranger : String
stranger =
    "0x00000000000000000000000000000000000000ff"


{-| The swap's edge whose leg transactions move value between `legAddress` and
the settlement contract.
-}
edgeWith : String -> Api.Data.ExternalConversion -> ConversionEdge
edgeWith legAddress raw =
    ConversionEdge.init raw
        ( Id.init "bnb" inputLegId, Id.init "bnb" outputLegId )
        ( Id.init "bnb" swapper, Id.init "bnb" swapper )
        (accountTx inputLegId legAddress settlement)
        (accountTx outputLegId settlement legAddress)


edge : Api.Data.ExternalConversion -> ConversionEdge
edge =
    edgeWith swapper


{-| The conversion's `fromAddress`/`toAddress` is on neither leg transaction.
-}
mismatchedEdge : Api.Data.ExternalConversion -> ConversionEdge
mismatchedEdge =
    edgeWith stranger


usd6 : List Api.Data.Rate
usd6 =
    [ { code = "usd", value = 6 } ]


{-| ac-eth-fill-from-base as recorded (var/bridge-maps/v2/api): native ETH
on base to native ETH on eth, both legs bare tx hashes (D-37).
-}
acrossFillFromBase : Api.Data.ExternalConversion
acrossFillFromBase =
    { dexSwap
        | conversionType = Api.Data.ExternalConversionConversionTypeBridgeTx
        , fromAddress = depositor
        , toAddress = depositor
        , fromAsset = ConversionEdge.nativeAsset
        , toAsset = ConversionEdge.nativeAsset
        , fromAmount = "0x3f67c07f4e7000"
        , toAmount = "0x3f02f5eaecdb25"
        , fromAssetTransfer = depositHash
        , toAssetTransfer = fillHash
        , fromNetwork = "base"
        , toNetwork = "eth"
        , protocol = Just "across"
        , transferId = Just "5154354"
    }


depositor : String
depositor =
    "0xa9c9259ca0efe93e2af7bda7d8f438683d4b1fd6"


depositHash : String
depositHash =
    "2c1cfd091e7f5f56854abf49d275cac5ec7eaba29aad86ea7e381eb1b1ef2542"


fillHash : String
fillHash =
    "a89e63d97f3add6174a939845fdb8078da3f0e4a17c830b9f291176efcbddcb8"


spokePool : String
spokePool =
    "0x5c7bcd6e7de5423a257d81b442095a1a6ced35c5"


{-| A stand-in, not the fill's real relayer.
-}
relayer : String
relayer =
    "0x07ae8551be970cb1cca11dd7a11f47ae82e70e67"


{-| A whole tx as `/txs/{hash}` serves it: its root transfer.
-}
rootTx : String -> String -> String -> String -> Api.Data.Values -> Api.Data.Tx
rootTx network hash_ from to value =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = "eth"
        , fee = Nothing
        , fromAddress = from
        , height = 1
        , identifier = hash_ ++ "_I0"
        , isExternal = Just True
        , network = network
        , timestamp = 1788254088
        , toAddress = to
        , tokenTxId = Nothing
        , txHash = hash_
        , txType = "account"
        , value = value
        }


{-| The depositor calls the SpokePool directly with the bridged amount.
-}
baseDeposit : Api.Data.Tx
baseDeposit =
    rootTx "base" depositHash depositor spokePool { value = 0x003F67C07F4E7000, fiatValues = usd6 }


{-| The relayer calls the SpokePool with no value; the SpokePool pays the
depositor through an internal transfer, so the root moves nothing to them.
-}
ethFill : Api.Data.Tx
ethFill =
    rootTx "eth" fillHash relayer spokePool { value = 0, fiatValues = [] }


acrossEdge : Api.Data.Tx -> Api.Data.Tx -> ConversionEdge
acrossEdge deposit fill =
    ConversionEdge.init acrossFillFromBase
        ( Id.init "base" (depositHash ++ "_I0"), Id.init "eth" (fillHash ++ "_I0") )
        ( Id.init "base" depositor, Id.init "eth" depositor )
        deposit
        fill


suite : Test
suite =
    describe "Model.Pathfinder.ConversionEdge leg values"
        [ describe "native bridge legs named by their bare hash (ac-eth-fill-from-base)"
            [ test "the output leg is the row's amount, not the fill tx's zero value" <|
                \_ ->
                    acrossEdge baseDeposit ethFill
                        |> ConversionEdge.outputAmount
                        |> Expect.equal
                            (ConversionEdge.Amount (Currency.assetFromBase "eth")
                                { value = 0x003F02F5EAECDB25, fiatValues = [] }
                            )
            , test "the input leg keeps the deposit tx's amount and fiat" <|
                \_ ->
                    acrossEdge baseDeposit ethFill
                        |> ConversionEdge.inputAmount
                        |> Expect.equal
                            (ConversionEdge.Amount (Currency.assetFromBase "base")
                                { value = 0x003F67C07F4E7000, fiatValues = usd6 }
                            )
            , test "a deposit tx moving more than the bridged amount shows the row's amount, with the tx's fiat scaled to it" <|
                \_ ->
                    -- the row carries no quote, as the backend sends it today:
                    -- the fiat must not go blank
                    acrossEdge
                        (rootTx "base" depositHash depositor spokePool { value = 0x003F67C07F4E7000 * 2, fiatValues = usd6 })
                        ethFill
                        |> ConversionEdge.inputAmount
                        |> Expect.equal
                            (ConversionEdge.Amount (Currency.assetFromBase "base")
                                { value = 0x003F67C07F4E7000, fiatValues = [ { code = "usd", value = 3 } ] }
                            )
            , test "the row's own quote wins over the scaled tx fiat" <|
                \_ ->
                    ConversionEdge.init { acrossFillFromBase | fromAmountFiatValues = Just [ { code = "usd", value = 4 } ] }
                        ( Id.init "base" (depositHash ++ "_I0"), Id.init "eth" (fillHash ++ "_I0") )
                        ( Id.init "base" depositor, Id.init "eth" depositor )
                        (rootTx "base" depositHash depositor spokePool { value = 0x003F67C07F4E7000 * 2, fiatValues = usd6 })
                        ethFill
                        |> ConversionEdge.inputAmount
                        |> Expect.equal
                            (ConversionEdge.Amount (Currency.assetFromBase "base")
                                { value = 0x003F67C07F4E7000, fiatValues = [ { code = "usd", value = 4 } ] }
                            )
            , test "a tx value a rounding step off the row's amount is the same amount" <|
                \_ ->
                    -- above 2^53 the row's hex and the JSON number can round a
                    -- unit in the last place apart (2048 base units here); the
                    -- tx then keeps its own value and fiat
                    let
                        rounded =
                            0x003F67C07F4E7000 * 1024

                        tx =
                            rootTx "base" depositHash depositor spokePool { value = rounded + 2048, fiatValues = usd6 }
                    in
                    ConversionEdge.init { acrossFillFromBase | fromAmount = "0xfd9f01fd39c00000" }
                        ( Id.init "base" (depositHash ++ "_I0"), Id.init "eth" (fillHash ++ "_I0") )
                        ( Id.init "base" depositor, Id.init "eth" depositor )
                        tx
                        ethFill
                        |> ConversionEdge.inputAmount
                        |> Expect.equal
                            (ConversionEdge.Amount (Currency.assetFromBase "base")
                                { value = rounded + 2048, fiatValues = usd6 }
                            )
            ]
        , test "the conversion's own quote wins over the leg transaction's" <|
            \_ ->
                edge { dexSwap | toAmountFiatValues = Just usd6 }
                    |> ConversionEdge.outputValues
                    |> Expect.equal { fiatValues = usd6, value = 1 }
        , test "without a quote the leg transaction's own values are untouched" <|
            \_ ->
                edge dexSwap
                    |> ConversionEdge.outputValues
                    |> Expect.equal { fiatValues = [], value = 1 }
        , test "the input leg's own quote wins too" <|
            \_ ->
                edge { dexSwap | fromAmountFiatValues = Just usd6 }
                    |> ConversionEdge.inputValues
                    |> Expect.equal { fiatValues = usd6, value = 1 }
        , test "without a quote the input leg's own values are untouched" <|
            \_ ->
                edge dexSwap
                    |> ConversionEdge.inputValues
                    |> Expect.equal { fiatValues = [], value = 1 }
        , test "an address-mismatched leg carries no fiat beside its zero amount" <|
            \_ ->
                mismatchedEdge { dexSwap | fromAmountFiatValues = Just usd6 }
                    |> ConversionEdge.inputValues
                    |> .fiatValues
                    |> Expect.equal []
        , test "an address-mismatched output leg carries no fiat either" <|
            \_ ->
                mismatchedEdge { dexSwap | toAmountFiatValues = Just usd6 }
                    |> ConversionEdge.outputValues
                    |> .fiatValues
                    |> Expect.equal []
        , describe "a leg that does not touch the row's address (swap paid through an intermediary, pol 0x1733adc7)"
            [ test "the output shows the row's amount and quote, not zero" <|
                \_ ->
                    mismatchedEdge throughForwarder
                        |> ConversionEdge.outputAmount
                        |> Expect.equal
                            (ConversionEdge.Amount (Currency.asset "bnb" usd1Contract)
                                { value = 196481686, fiatValues = usd6 }
                            )
            , test "the input shows the row's amount and quote, not zero" <|
                \_ ->
                    mismatchedEdge throughForwarder
                        |> ConversionEdge.inputAmount
                        |> Expect.equal
                            (ConversionEdge.Amount (Currency.asset "bnb" busdContract)
                                { value = 390282558, fiatValues = usd6 }
                            )
            , test "a leg that touches the address still shows the leg's own value" <|
                \_ ->
                    edge throughForwarder
                        |> ConversionEdge.outputAmount
                        |> amountValue
                        |> Expect.equal (Just 1)
            ]
        ]


{-| The swap row of pol 0x1733adc7 in shape: its output leg is the swap's output
Transfer, which paid the intermediary (the deBridge forwarder) and not the row's
`toAddress`. Amounts small enough for exact Ints.
-}
throughForwarder : Api.Data.ExternalConversion
throughForwarder =
    { dexSwap
        | fromAmount = "0x17433d3e"
        , toAmount = "0xbb61296"
        , fromAmountFiatValues = Just usd6
        , toAmountFiatValues = Just usd6
    }


busdContract : String
busdContract =
    "0xe9e7cea3dedca5984780bafc599bd69add087d56"


amountValue : ConversionEdge.LegAmount -> Maybe Int
amountValue legAmount =
    case legAmount of
        ConversionEdge.Amount _ values ->
            Just values.value

        ConversionEdge.NotInRegistry ->
            Nothing
