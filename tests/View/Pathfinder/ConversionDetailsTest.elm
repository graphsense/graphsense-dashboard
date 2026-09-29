module View.Pathfinder.ConversionDetailsTest exposing (suite)

{-| A bridge moves value across networks, so its output leg is a transaction on
the DESTINATION network: its amount must be read as that network's coin. Read
on the source network, a btc output of a bridge from eth is keyed to eth.
-}

import Api.Data
import Expect
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Support.Env as Env
import Test exposing (Test, describe, test)
import View.Pathfinder.ConversionDetails as ConversionDetails
import View.Pathfinder.Details exposing (valuesToCell)


sender : String
sender =
    "0x1c1df1eb43bb46e1f6e1a4d59bd15ddbf7a0cdaf"


bridgeContract : String
bridgeContract =
    "0x0a0c1a9cbaef41e1e5a5b4e34d4b2a49f8b7e2b1"


receiver : String
receiver =
    "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"


inputLeg : Api.Data.Tx
inputLeg =
    Api.Data.TxTxAccount
        { contractCreation = Nothing
        , currency = "eth"
        , fee = Nothing
        , fromAddress = sender
        , height = 20000000
        , identifier = "aaa_I0"
        , isExternal = Nothing
        , network = "eth"
        , timestamp = 1718000000
        , toAddress = bridgeContract
        , tokenTxId = Nothing
        , txHash = "aaa"
        , txType = "account"
        , value = { fiatValues = [], value = 500000000000000000 }
        }


{-| 1.5 BTC paid out to the receiver on the destination network.
-}
outputLeg : Api.Data.Tx
outputLeg =
    Api.Data.TxTxUtxo
        { coinbase = False
        , currency = "btc"
        , height = 850000
        , inputs = Just []
        , noInputs = 1
        , noOutputs = 1
        , outputs =
            Just
                [ { address = [ receiver ]
                  , index = Just 0
                  , value = { fiatValues = [], value = 150000000 }
                  }
                ]
        , timestamp = 1718000600
        , totalInput = { fiatValues = [], value = 150010000 }
        , totalOutput = { fiatValues = [], value = 150000000 }
        , txHash = "bbb"
        , txType = "utxo"
        , heuristics = Nothing
        }


bridge : Api.Data.ExternalConversion
bridge =
    { conversionType = Api.Data.ExternalConversionConversionTypeBridgeTx
    , fromAddress = sender
    , fromAmount = "500000000000000000"
    , fromAsset = "native"
    , fromAssetTransfer = "aaa_I0"
    , fromIsSupportedAsset = True
    , fromNetwork = "eth"
    , toAddress = receiver
    , toAmount = "150000000"
    , toAsset = "native"
    , toAssetTransfer = "bbb"
    , toIsSupportedAsset = True
    , toNetwork = "btc"
    , fromAssetSymbol = Nothing
    , toAssetSymbol = Nothing
    , fromAssetDecimals = Nothing
    , toAssetDecimals = Nothing
    , fromAmountFiatValues = Nothing
    , toAmountFiatValues = Nothing
    }


edge : ConversionEdge
edge =
    ConversionEdge.init bridge
        ( Id.init "eth" "aaa_I0", Id.init "btc" "bbb" )
        ( Id.init "eth" sender, Id.init "btc" receiver )
        inputLeg
        outputLeg


suite : Test
suite =
    describe "View.Pathfinder.ConversionDetails"
        [ test "a bridge's output leg is valued on the destination network" <|
            \_ ->
                let
                    asset =
                        ConversionDetails.outputAsset edge
                in
                ( asset
                , (ConversionEdge.outputValues edge |> valuesToCell Env.viewConfig asset).firstRowText
                )
                    |> Expect.equal
                        ( { network = "btc", asset = "btc" }
                        , "1.50 BTC"
                        )
        ]
