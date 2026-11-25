module View.Pathfinder.ConversionEdgeLabelTest exposing (suite)

{-| The second label line of a swap or bridge edge names both legs' assets. A
native leg is the network's gas coin, which on an L2 is not the network code:
on arb it is ETH, while "ARB" is a token of its own.
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
accountTx =
    txOn "arb" "arb"


txOn : String -> String -> String -> Api.Data.Tx
txOn network currency identifier =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = currency
        , fee = Nothing
        , fromAddress = swapper
        , height = 200000000
        , identifier = identifier
        , isExternal = Nothing
        , network = network
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
        , protocol = Nothing
        , protocolApp = Nothing
        , transferId = Nothing
        }
        ( Id.init "arb" "aaa_I0", Id.init "arb" "aaa_T1" )
        ( Id.init "arb" swapper, Id.init "arb" swapper )
        (accountTx "aaa_I0")
        (accountTx "aaa_T1")


type alias BridgeLeg =
    { network : String, rawAsset : String, txCurrency : String }


bridge : BridgeLeg -> BridgeLeg -> ConversionEdge
bridge from to =
    ConversionEdge.init
        { conversionType = Api.Data.ExternalConversionConversionTypeBridgeTx
        , fromAddress = swapper
        , fromAmount = "1"
        , fromAsset = from.rawAsset
        , fromAssetTransfer = "0xaaa"
        , fromIsSupportedAsset = True
        , fromNetwork = from.network
        , toAddress = swapper
        , toAmount = "1"
        , toAsset = to.rawAsset
        , toAssetTransfer = "0xbbb"
        , toIsSupportedAsset = True
        , toNetwork = to.network
        , fromAssetSymbol = Nothing
        , toAssetSymbol = Nothing
        , fromAssetDecimals = Nothing
        , toAssetDecimals = Nothing
        , fromAmountFiatValues = Nothing
        , toAmountFiatValues = Nothing
        , protocol = Nothing
        , protocolApp = Nothing
        , transferId = Nothing
        }
        ( Id.init from.network "aaa_I0", Id.init to.network "bbb_I0" )
        ( Id.init from.network swapper, Id.init to.network swapper )
        (txOn from.network from.txCurrency "aaa_I0")
        (txOn to.network to.txCurrency "bbb_I0")


label : ConversionEdge -> String
label =
    ConversionEdgeView.assetsLabel Env.viewConfig.locale


suite : Test
suite =
    describe "conversion edge asset label"
        [ swapSuite, bridgeSuite ]


bridgeSuite : Test
bridgeSuite =
    describe "bridge edge asset label"
        [ test "native legs on L2s read as their gas coin, keeping the network prefix" <|
            \_ ->
                bridge
                    { network = "arb", rawAsset = "native", txCurrency = "arb" }
                    { network = "base", rawAsset = "native", txCurrency = "base" }
                    |> label
                    |> Expect.equal "ARB-ETH / BASE-ETH"
        , test "a native eth leg to a native gnosis leg" <|
            \_ ->
                bridge
                    { network = "eth", rawAsset = "native", txCurrency = "eth" }
                    { network = "gnosis", rawAsset = "native", txCurrency = "gnosis" }
                    |> label
                    |> Expect.equal "ETH-ETH / GNOSIS-XDAI"
        , test "token legs keep the loaded tx's asset" <|
            \_ ->
                bridge
                    { network = "arb", rawAsset = usdc, txCurrency = "usdc" }
                    { network = "eth", rawAsset = "0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48", txCurrency = "usdc" }
                    |> label
                    |> Expect.equal "ARB-USDC / ETH-USDC"
        , test "a native leg next to a token leg resolves only the native side" <|
            \_ ->
                bridge
                    { network = "opt", rawAsset = "native", txCurrency = "opt" }
                    { network = "eth", rawAsset = "0xdac17f958d2ee523a2206206994597c13d831ec7", txCurrency = "usdt" }
                    |> label
                    |> Expect.equal "OPT-ETH / ETH-USDT"
        , test "a legacy leg naming its currency code keeps the tx's asset" <|
            \_ ->
                bridge
                    { network = "eth", rawAsset = "eth", txCurrency = "eth" }
                    { network = "trx", rawAsset = "usdt", txCurrency = "usdt" }
                    |> label
                    |> Expect.equal "ETH-ETH / TRX-USDT"
        ]


swapSuite : Test
swapSuite =
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
