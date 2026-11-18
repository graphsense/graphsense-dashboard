module Model.Pathfinder.ConversionEdgeTest exposing (suite)

import Api.Data
import Expect
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Model.Currency as Currency
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Support.SwapFixture exposing (accountTx, dexSwap, inputLegId, outputLegId, settlement, swapper)
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
            , test "a deposit tx moving more than the bridged amount shows the row's amount" <|
                \_ ->
                    acrossEdge
                        (rootTx "base" depositHash depositor spokePool { value = 0x003F67C07F4E7000 + 1000, fiatValues = usd6 })
                        ethFill
                        |> ConversionEdge.inputAmount
                        |> Expect.equal
                            (ConversionEdge.Amount (Currency.assetFromBase "base")
                                { value = 0x003F67C07F4E7000, fiatValues = [] }
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
        ]
