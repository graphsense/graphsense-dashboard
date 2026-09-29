module Support.SwapFixture exposing
    ( accountTx
    , dexSwap
    , hash
    , inputLegId
    , outputLegId
    , settlement
    , swapper
    , txRequest
    , usd1Contract
    )

{-| One dex swap inside one bnb tx: `swapper` sends BUSD on the input leg and
gets USD1 back on the output leg.
-}

import Api.Data
import Model.Pathfinder.Network exposing (FindPosition(..))
import Msg.Pathfinder exposing (AddingTxConfig)


hash : String
hash =
    "63336a5ace33dc969cdb769f64b8499eae7f142741895fa4d589dbfa41bf5d95"


swapper : String
swapper =
    "0xe17ad4f88be9cd479a8036a058b893a0da62bc7a"


settlement : String
settlement =
    "0x9008d19f58aabd9ed0d60971565aa8510560ab41"


inputLegId : String
inputLegId =
    hash ++ "_T95"


outputLegId : String
outputLegId =
    hash ++ "_T108"


usd1Contract : String
usd1Contract =
    "0x8d0d000ee44948fc98c9b98a4fa4921476f08b0d"


{-| A sub-tx of the swap's tx moving one base unit from `from` to `to`.
-}
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
        , txHash = hash
        , txType = "account"
        , value = { fiatValues = [], value = 1 }
        }


{-| The swap as the API serves it without any of the optional per-leg keys;
its leg identifiers carry the `0x` prefix, as served.
-}
dexSwap : Api.Data.ExternalConversion
dexSwap =
    { conversionType = Api.Data.ExternalConversionConversionTypeDexSwap
    , fromAddress = swapper
    , fromAmount = "0x16a4ecb955b8a31b"
    , fromAsset = "0xe9e7cea3dedca5984780bafc599bd69add087d56"
    , fromAssetTransfer = "0x" ++ inputLegId
    , fromIsSupportedAsset = True
    , fromNetwork = "bnb"
    , toAddress = swapper
    , toAmount = "0x16a56e085c4dad4f"
    , toAsset = usd1Contract
    , toAssetTransfer = "0x" ++ outputLegId
    , toIsSupportedAsset = True
    , toNetwork = "bnb"
    , fromAssetSymbol = Nothing
    , toAssetSymbol = Nothing
    , fromAssetDecimals = Nothing
    , toAssetDecimals = Nothing
    , fromAmountFiatValues = Nothing
    , toAmountFiatValues = Nothing
    }


{-| How an answered tx request for `requestedTxHash` puts the tx on the graph.
-}
txRequest : String -> AddingTxConfig
txRequest requestedTxHash =
    { pos = Auto
    , loadAddresses = False
    , autoLinkInTraceMode = False
    , requestedTxHash = requestedTxHash
    }
