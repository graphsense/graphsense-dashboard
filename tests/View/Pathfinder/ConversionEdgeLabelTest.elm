module View.Pathfinder.ConversionEdgeLabelTest exposing (suite)

{-| The second label line of a swap edge names both legs' assets. A native leg
is the network's gas coin, which on an L2 is not the network code: on arb it
is ETH, while "ARB" is a token of its own.
-}

import Api.Data
import Expect
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Model.Pathfinder.ConversionEdge exposing (ConversionEdge)
import Support.Env as Env
import Test exposing (Test, describe, test)
import View.Pathfinder.ConversionEdge as ConversionEdgeView


swapper : String
swapper =
    "0xe17ad4f88be9cd479a8036a058b893a0da62bc7a"


arbToken : String
arbToken =
    "0x912ce59144191c1204e64559fe8253a0e49e6548"


usdc : String
usdc =
    "0xaf88d065e77c8cc2239327c5edb3a432268e5831"


accountTx : String -> Api.Data.Tx
accountTx identifier =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = "arb"
        , fee = Nothing
        , fromAddress = swapper
        , height = 200000000
        , identifier = identifier
        , isExternal = Nothing
        , network = "arb"
        , timestamp = 1718000000
        , toAddress = swapper
        , tokenTxId = Nothing
        , txHash = "aaa"
        , txType = "account"
        , value = { fiatValues = [], value = 1 }
        }


swap : { toAsset : String, toAssetSymbol : Maybe String } -> ConversionEdge
swap { toAsset, toAssetSymbol } =
    ConversionEdge.init
        { conversionType = Api.Data.ExternalConversionConversionTypeDexSwap
        , fromAddress = swapper
        , fromAmount = "1"
        , fromAsset = "native"
        , fromAssetTransfer = "aaa_I0"
        , fromIsSupportedAsset = True
        , fromNetwork = "arb"
        , toAddress = swapper
        , toAmount = "1"
        , toAsset = toAsset
        , toAssetTransfer = "aaa_T1"
        , toIsSupportedAsset = True
        , toNetwork = "arb"
        , fromAssetSymbol = Nothing
        , toAssetSymbol = toAssetSymbol
        , fromAssetDecimals = Nothing
        , toAssetDecimals = Nothing
        , fromAmountFiatValues = Nothing
        , toAmountFiatValues = Nothing
        }
        ( Id.init "arb" "aaa_I0", Id.init "arb" "aaa_T1" )
        ( Id.init "arb" swapper, Id.init "arb" swapper )
        (accountTx "aaa_I0")
        (accountTx "aaa_T1")


label : ConversionEdge -> String
label =
    ConversionEdgeView.assetsLabel Env.viewConfig.locale


suite : Test
suite =
    describe "swap edge asset label"
        [ test "an arb native leg reads as the gas coin, not the network code" <|
            \_ ->
                swap { toAsset = usdc, toAssetSymbol = Just "USDC" }
                    |> label
                    |> Expect.equal "ETH / USDC"
        , test "an arb native to ARB-token swap names both assets apart" <|
            \_ ->
                swap { toAsset = arbToken, toAssetSymbol = Just "ARB" }
                    |> label
                    |> Expect.equal "ETH / ARB"
        , test "a token leg without any ticker shows its shortened address" <|
            \_ ->
                swap { toAsset = usdc, toAssetSymbol = Nothing }
                    |> label
                    |> String.contains (String.toUpper usdc)
                    |> Expect.equal False
        ]
