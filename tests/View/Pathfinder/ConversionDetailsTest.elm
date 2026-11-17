module View.Pathfinder.ConversionDetailsTest exposing (suite)

{-| A bridge moves value across networks, so its output leg is a transaction on
the DESTINATION network: its amount must be read as that network's coin. Read
on the source network, a btc output of a bridge from eth is keyed to eth.

The panel also names the bridge the API says proves the pair (`protocol`,
`protocol_app`, `transfer_id`, adapter D-38) and links both legs' chain
explorers and the protocol's own explorer.

-}

import Api.Data
import Expect
import Html.Attributes
import Html.Styled
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
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
    , protocol = Nothing
    , protocolApp = Nothing
    , transferId = Nothing
    }


edge : ConversionEdge
edge =
    ConversionEdge.init bridge
        ( Id.init "eth" "aaa_I0", Id.init "btc" "bbb" )
        ( Id.init "eth" sender, Id.init "btc" receiver )
        inputLeg
        outputLeg


usdt0Guid : String
usdt0Guid =
    "0xb071cfdf0f4e7c9bfff73e34fdc70f894bf8e799ce674722b1b15e35bf8c500d"


{-| The adapter's row for the USDT0 transfer arb 0x507fa262… → eth 0xcfa572bc….
-}
usdt0 : Api.Data.ExternalConversion
usdt0 =
    { bridge
        | fromAssetTransfer = "507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0_T13"
        , fromNetwork = "arb"
        , toAssetTransfer = "cfa572bc24099007fdcef331b17d30e83673b7c6972ec1c04419afba9b58a720_T658"
        , toNetwork = "eth"
        , protocol = Just "layerzero"
        , protocolApp = Just "usdt0"
        , transferId = Just usdt0Guid
    }


edgeOf : Api.Data.ExternalConversion -> ConversionEdge
edgeOf c =
    ConversionEdge.init c
        ( Id.init c.fromNetwork c.fromAssetTransfer, Id.init c.toNetwork c.toAssetTransfer )
        ( Id.init c.fromNetwork sender, Id.init c.toNetwork receiver )
        inputLeg
        outputLeg


rows : Api.Data.ExternalConversion -> Query.Single ()
rows =
    edgeOf >> ConversionDetails.bridgeRows Env.viewConfig >> Html.Styled.toUnstyled >> Query.fromHtml


explorerLinks : Api.Data.ExternalConversion -> Query.Multiple ()
explorerLinks =
    rows >> Query.findAll [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-bridge-explorer-link") ]


suite : Test
suite =
    describe "View.Pathfinder.ConversionDetails"
        [ describe "which bridge, and where to look it up"
            [ test "the title stays the kind of transaction" <|
                \_ ->
                    [ usdt0, bridge ]
                        |> List.map (ConversionDetails.title Env.viewConfig)
                        |> Expect.equal [ "Bridge transaction", "Bridge transaction" ]
            , test "the protocol, and the app on top of it, are named as people know them" <|
                \_ ->
                    [ usdt0
                    , { usdt0 | protocolApp = Nothing }
                    , { usdt0 | protocol = Just "debridge", protocolApp = Nothing }
                    , { usdt0 | protocol = Nothing }
                    , bridge
                    , { usdt0 | conversionType = Api.Data.ExternalConversionConversionTypeDexSwap }
                    ]
                        |> List.map ConversionDetails.protocolLabel
                        |> Expect.equal [ Just "LayerZero · USDT0", Just "LayerZero", Just "deBridge", Nothing, Nothing, Nothing ]
            , test "blank protocols add no rows, and blank apps add no separator" <|
                \_ ->
                    Expect.all
                        [ \_ ->
                            [ "", "  " ]
                                |> List.map (\name -> ConversionDetails.protocolLabel { usdt0 | protocol = Just name })
                                |> Expect.equal [ Nothing, Nothing ]
                        , \_ ->
                            [ "", "  " ]
                                |> List.map (\name -> ConversionDetails.protocolLabel { usdt0 | protocolApp = Just name })
                                |> Expect.equal [ Just "LayerZero", Just "LayerZero" ]
                        ]
                        ()
            , test "known metadata tolerates whitespace and casing in names and explorer lookup" <|
                \_ ->
                    rows { usdt0 | protocol = Just " LayerZERO ", protocolApp = Just " UsDt0 " }
                        |> Expect.all
                            [ Query.has [ Selector.text "LayerZero · USDT0" ]
                            , Query.has [ Selector.text "LayerZero Scan" ]
                            ]
            , test "a malformed explorer id preserves the protocol row" <|
                \_ ->
                    rows { usdt0 | protocol = Just "debridge", protocolApp = Nothing, transferId = Just "0x12" }
                        |> Expect.all
                            [ Query.has [ Selector.text "deBridge" ]
                            , Query.hasNot [ Selector.text "Bridge explorer" ]
                            ]
            , test "the protocol is a row of the details table, above its explorer" <|
                -- a row like Sender or Timestamp, not a line under the title;
                -- the legs are listed, copyable and linked in the Bridge
                -- transactions section below, so the panel does not repeat them
                \_ ->
                    rows usdt0
                        |> Expect.all
                            [ Query.has [ Selector.text "Protocol", Selector.text "LayerZero · USDT0" ]
                            , Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-bridge-protocol") ]
                                >> Query.has [ Selector.text "LayerZero · USDT0" ]
                            , Query.has [ Selector.text "Bridge explorer", Selector.text "LayerZero Scan" ]
                            , Query.has [ Selector.attribute (Html.Attributes.href "https://layerzeroscan.com/tx/0x507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0") ]
                            , Query.hasNot [ Selector.text "Source tx" ]
                            , Query.hasNot [ Selector.text "Destination tx" ]
                            , Query.hasNot [ Selector.text "Transfer ID" ]
                            ]
            , test "the bridge explorer opens a new tab and sends no referrer" <|
                \_ ->
                    explorerLinks usdt0
                        |> Expect.all
                            [ Query.count (Expect.equal 1)
                            , Query.each (Query.has [ Selector.attribute (Html.Attributes.target "_blank") ])
                            , Query.each (Query.has [ Selector.attribute (Html.Attributes.rel "noopener noreferrer") ])
                            ]
            , test "a CCTP transfer links Range's explorer, which shows both legs" <|
                -- Circle runs no explorer; Range's search finds a transfer by its
                -- source tx (agent/notes/cctp-range-linking in the adapter repo)
                \_ ->
                    rows { usdt0 | protocol = Just "cctp", protocolApp = Nothing, transferId = Just "506370" }
                        |> Expect.all
                            [ Query.has [ Selector.text "Bridge explorer", Selector.text "Range CCTP Explorer" ]
                            , Query.has [ Selector.attribute (Html.Attributes.href "https://usdc.range.org/transactions?s=0x507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0") ]
                            ]
            , test "a protocol without an explorer still names the protocol" <|
                \_ ->
                    rows { usdt0 | protocol = Just "symbiosis", protocolApp = Nothing, transferId = Nothing }
                        |> Expect.all
                            [ Query.has [ Selector.text "Protocol", Selector.text "Symbiosis" ]
                            , Query.hasNot [ Selector.text "Bridge explorer" ]
                            ]
            , test "a row without a protocol adds nothing" <|
                -- not even an empty block: the panel looks as it did before
                \_ ->
                    rows { usdt0 | protocol = Nothing, protocolApp = Nothing, transferId = Nothing }
                        |> Query.hasNot [ Selector.tag "div" ]
            , test "a swap shows none of it" <|
                \_ ->
                    rows { usdt0 | conversionType = Api.Data.ExternalConversionConversionTypeDexSwap }
                        |> Expect.all
                            [ Query.hasNot [ Selector.text "Bridge explorer" ]
                            , Query.hasNot [ Selector.text "Protocol" ]
                            ]
            ]
        , test "a bridge's output leg is valued on the destination network" <|
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
