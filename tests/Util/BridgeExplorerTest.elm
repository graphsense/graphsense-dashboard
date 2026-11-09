module Util.BridgeExplorerTest exposing (suite)

{-| The bridge panel names the protocol a bridge row went through and links the
protocol's own explorer. The links are built here from the row's legs and its
transfer id; a malformed id never becomes a URL.
-}

import Expect
import Test exposing (Test, describe, test)
import Util.BridgeExplorer as BridgeExplorer


usdt0Send : String
usdt0Send =
    "507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0_T13"


{-| The order of bnb 0x2b22eab7… → eth 0x9254eb94… (the adapter's recorded case).
-}
debridgeOrder : String
debridgeOrder =
    "0xe568758c8490ec89da01036343920ee243b56e227c13adb9887d02cdb5df6128"


thorchainDeposit : String
thorchainDeposit =
    "42d529a72cecd6ece546d5ac0d2a6c2a9407876b66478a33917d8928833433f8_I0"


{-| The source leg of the CCTP V2 transfer eth 0xe8c9cfb2… → arb 0xaf516e4c….
-}
cctpBurn : String
cctpBurn =
    "e8c9cfb2ee4a139c45173e27784b669e9741fb04418475cd7f8dfbf0124202af_T693"


link : String -> Maybe String -> Maybe ( String, String )
link protocol transferId =
    BridgeExplorer.explorerLink { protocol = protocol, sourceLeg = usdt0Send, transferId = transferId }
        |> Maybe.map (\l -> ( l.url, l.label ))


suite : Test
suite =
    describe "Util.BridgeExplorer"
        [ describe "names"
            [ test "each protocol the API sends has the name people use" <|
                \_ ->
                    [ "cctp", "debridge", "layerzero", "thorchain", "symbiosis" ]
                        |> List.map BridgeExplorer.protocolName
                        |> Expect.equal [ "CCTP", "deBridge", "LayerZero", "THORChain", "Symbiosis" ]
            , test "an app on top of LayerZero" <|
                \_ ->
                    [ "stargate", "usdt0" ]
                        |> List.map BridgeExplorer.appName
                        |> Expect.equal [ "Stargate", "USDT0" ]
            , test "an unknown name is shown as the API spells it" <|
                \_ ->
                    ( BridgeExplorer.protocolName "wormhole", BridgeExplorer.appName "oft-x" )
                        |> Expect.equal ( "wormhole", "oft-x" )
            ]
        , describe "the leg's transaction hash"
            [ test "drops the leg suffix" <|
                \_ ->
                    BridgeExplorer.legTxHash usdt0Send
                        |> Expect.equal "507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0"
            , test "drops a 0x prefix and keeps a bare hash" <|
                \_ ->
                    [ "0xABCDEF_I2", "abcdef" ]
                        |> List.map BridgeExplorer.legTxHash
                        |> Expect.equal [ "ABCDEF", "abcdef" ]
            ]
        , describe "the protocol's explorer"
            [ test "LayerZero Scan finds the message by its source tx" <|
                \_ ->
                    link "layerzero" Nothing
                        |> Expect.equal
                            (Just
                                ( "https://layerzeroscan.com/tx/0x507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0"
                                , "LayerZero Scan"
                                )
                            )
            , test "deExplorer opens the order by its id (app.debridge.finance only redirects there)" <|
                \_ ->
                    link "debridge" (Just debridgeOrder)
                        |> Expect.equal
                            (Just
                                ( "https://app.debridge.com/order?orderId=" ++ debridgeOrder
                                , "deExplorer"
                                )
                            )
            , test "deBridge without an order id has no link" <|
                \_ ->
                    link "debridge" Nothing
                        |> Expect.equal Nothing
            , test "a transfer id that is not an order id never becomes a URL" <|
                \_ ->
                    [ "0x12", debridgeOrder ++ "&x=1", "javascript:alert(1)" ]
                        |> List.map (Just >> link "debridge")
                        |> Expect.equal [ Nothing, Nothing, Nothing ]
            , test "thorchain.net takes the inbound hash in capitals, without 0x" <|
                \_ ->
                    BridgeExplorer.explorerLink { protocol = "thorchain", sourceLeg = thorchainDeposit, transferId = Nothing }
                        |> Maybe.map (\l -> ( l.url, l.label ))
                        |> Expect.equal
                            (Just
                                ( "https://thorchain.net/tx/42D529A72CECD6ECE546D5AC0D2A6C2A9407876B66478A33917D8928833433F8"
                                , "THORChain Explorer"
                                )
                            )
            , test "a source leg that is not a tx hash never becomes a URL" <|
                \_ ->
                    BridgeExplorer.explorerLink { protocol = "layerzero", sourceLeg = "/../evil_T1", transferId = Nothing }
                        |> Expect.equal Nothing
            , test "Range's CCTP explorer finds the transfer by its source tx, as lowercase hex after 0x" <|
                -- without the 0x Range answers "No results found" (checked 2026-11-09)
                \_ ->
                    BridgeExplorer.explorerLink { protocol = "cctp", sourceLeg = String.toUpper cctpBurn, transferId = Just "0x33e1" }
                        |> Maybe.map (\l -> ( l.url, l.label ))
                        |> Expect.equal
                            (Just
                                ( "https://usdc.range.org/transactions?s=0xe8c9cfb2ee4a139c45173e27784b669e9741fb04418475cd7f8dfbf0124202af"
                                , "Range CCTP Explorer"
                                )
                            )
            , test "a CCTP source leg that is not a tx hash never becomes a URL" <|
                \_ ->
                    BridgeExplorer.explorerLink { protocol = "cctp", sourceLeg = "abc&s=x_T1", transferId = Nothing }
                        |> Expect.equal Nothing
            , test "unknown protocols have no explorer link" <|
                \_ ->
                    [ link "symbiosis" Nothing, link "wormhole" Nothing ]
                        |> Expect.equal [ Nothing, Nothing ]
            ]
        ]
