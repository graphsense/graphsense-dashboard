module Model.Pathfinder.ConversionEdgeTest exposing (suite)

{-| A swap leg is rendered from the amounts of the LOADED leg transaction, but
the fiat quote the conversion itself carries is the better one: the backend
priced the leg at the swap's own asset and height, while the leg tx's own
`fiatValues` may be empty for a token the baseline cannot price.
-}

import Api.Data
import Expect
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Test exposing (Test, describe, test)


swapper : String
swapper =
    "0x1c1df1eb43bb46e1f6e1a4d59bd15ddbf7a0cdaf"


settlement : String
settlement =
    "0x0a0c1a9cbaef41e1e5a5b4e34d4b2a49f8b7e2b1"


accountTx : String -> String -> String -> Api.Data.Tx
accountTx identifier from to =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = "bnb"
        , fee = Nothing
        , fromAddress = from
        , height = 119317568
        , identifier = identifier
        , isExternal = Nothing
        , network = "bnb"
        , timestamp = 1788254088
        , toAddress = to
        , tokenTxId = Nothing
        , txHash = "0xdeadbeef"
        , txType = "account"
        , value = { fiatValues = [], value = 1 }
        }


conversion : Api.Data.ExternalConversion
conversion =
    { conversionType = Api.Data.ExternalConversionConversionTypeDexSwap
    , fromAddress = swapper
    , fromAmount = "0x16a4ecb955b8a31b"
    , fromAsset = "0xe9e7cea3dedca5984780bafc599bd69add087d56"
    , fromAssetTransfer = "0xaaa_T1"
    , fromIsSupportedAsset = True
    , fromNetwork = "bnb"
    , toAddress = swapper
    , toAmount = "0x16a56e085c4dad4f"
    , toAsset = "0x8d0d000ee44948fc98c9b98a4fa4921476f08b0d"
    , toAssetTransfer = "0xaaa_T2"
    , toIsSupportedAsset = True
    , toNetwork = "bnb"
    , fromAssetSymbol = Nothing
    , toAssetSymbol = Nothing
    , fromAssetDecimals = Nothing
    , toAssetDecimals = Nothing
    , fromAmountFiatValues = Nothing
    , toAmountFiatValues = Nothing
    }


edge : Api.Data.ExternalConversion -> ConversionEdge
edge raw =
    ConversionEdge.init raw
        ( Id.init "bnb" "aaa_T1", Id.init "bnb" "aaa_T2" )
        ( Id.init "bnb" swapper, Id.init "bnb" swapper )
        (accountTx "aaa_T1" swapper settlement)
        (accountTx "aaa_T2" settlement swapper)


suite : Test
suite =
    describe "Model.Pathfinder.ConversionEdge leg values"
        [ test "the conversion's own quote wins over the leg transaction's" <|
            \_ ->
                edge { conversion | toAmountFiatValues = Just [ { code = "usd", value = 6 } ] }
                    |> ConversionEdge.outputValues
                    |> Expect.equal { fiatValues = [ { code = "usd", value = 6 } ], value = 1 }
        , test "without a quote the leg transaction's own values are untouched" <|
            \_ ->
                edge conversion
                    |> ConversionEdge.outputValues
                    |> Expect.equal { fiatValues = [], value = 1 }
        , test "the input leg reads the same way" <|
            \_ ->
                ( edge { conversion | fromAmountFiatValues = Just [ { code = "usd", value = 6 } ] }
                    |> ConversionEdge.inputValues
                , edge conversion |> ConversionEdge.inputValues
                )
                    |> Expect.equal
                        ( { fiatValues = [ { code = "usd", value = 6 } ], value = 1 }
                        , { fiatValues = [], value = 1 }
                        )
        ]
