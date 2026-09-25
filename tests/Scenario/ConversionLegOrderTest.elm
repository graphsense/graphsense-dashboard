module Scenario.ConversionLegOrderTest exposing (suite)

{-| Which of a conversion's two transactions is the OUTPUT leg.

The leg fetch and the leg ordering both ask this. When they disagree the edge
is assembled back to front and, because ConversionEdge.init reads its labels
off the loaded transactions rather than off the conversion, a bnb->eth bridge
renders as "BNB-USDT / ETH-BNB".

-}

import Api.Data
import Expect
import Test exposing (Test, describe, test)
import Update.Pathfinder exposing (isOutputLegOf)


{-| A real Stargate leg pair: bnb USDT -> eth USDT, both legs ERC20, so each
`*AssetTransfer` names a `_T<log index>` SUB-transfer rather than a bare hash.
-}
tokenBridge : Api.Data.ExternalConversion
tokenBridge =
    { conversionType = Api.Data.ExternalConversionConversionTypeBridgeTx
    , fromAddress = "0x98fc13632ff112e4667fc4f21ae980571f122b5a"
    , fromAmount = "0x69e10de76676d0800000"
    , fromAsset = "0x55d398326f99059ff775485246999027b3197955"
    , fromAssetTransfer = bnbHash ++ "_T60"
    , fromIsSupportedAsset = True
    , fromNetwork = "bnb"
    , toAddress = "0x98fc13632ff112e4667fc4f21ae980571f122b5a"
    , toAmount = "0x745870e4ff"
    , toAsset = "0xdac17f958d2ee523a2206206994597c13d831ec7"
    , toAssetTransfer = ethHash ++ "_T840"
    , toIsSupportedAsset = True
    , toNetwork = "eth"
    }


{-| A THORChain shape: the deposit leg is NATIVE, so it names the root trace,
whose id equals the base tx id. This is the case the original comparison was
written for and must keep working.
-}
nativeBridge : Api.Data.ExternalConversion
nativeBridge =
    { tokenBridge
        | fromAssetTransfer = bnbHash ++ "_I0"
        , toAssetTransfer = ethHash ++ "_I0"
    }


bnbHash : String
bnbHash =
    "d6a24c8473f617a2937c41a8188cbfced028d404be1925eac741e26a9c3cc018"


ethHash : String
ethHash =
    "c78a696c501418dbb8c06a5d6de7eb50e1fab6eb96c4201c372660c75467b162"


suite : Test
suite =
    describe "conversion leg order"
        [ test "the settlement sub-transfer is the output leg" <|
            \_ ->
                isOutputLegOf tokenBridge ( "eth", ethHash ++ "_T840" )
                    |> Expect.equal True
        , test "the deposit sub-transfer is not the output leg" <|
            \_ ->
                isOutputLegOf tokenBridge ( "bnb", bnbHash ++ "_T60" )
                    |> Expect.equal False
        , test "the source tx opened by its BARE hash is not the output leg" <|
            -- the regression: `<hash>` never equals `<hash>_T60`, so a test
            -- against the INPUT leg misses here and the fallback decided it
            \_ ->
                isOutputLegOf tokenBridge ( "bnb", bnbHash )
                    |> Expect.equal False
        , test "the settlement tx opened by its bare hash is not mistaken for the output leg" <|
            -- same id shape on the other side: still a miss, and the caller
            -- must not promote it just because the NETWORK matches
            \_ ->
                isOutputLegOf tokenBridge ( "eth", ethHash )
                    |> Expect.equal False
        , test "a 0x prefix on either side does not change the answer" <|
            \_ ->
                isOutputLegOf tokenBridge ( "eth", "0x" ++ ethHash ++ "_T840" )
                    |> Expect.equal True
        , test "the network alone is not enough" <|
            \_ ->
                isOutputLegOf tokenBridge ( "eth", bnbHash ++ "_T60" )
                    |> Expect.equal False
        , test "a native root-trace leg still resolves (THORChain shape)" <|
            \_ ->
                isOutputLegOf nativeBridge ( "eth", ethHash ++ "_I0" )
                    |> Expect.equal True
        , test "and its deposit side still does not" <|
            \_ ->
                isOutputLegOf nativeBridge ( "bnb", bnbHash ++ "_I0" )
                    |> Expect.equal False
        ]
