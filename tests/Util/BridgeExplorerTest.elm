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


{-| The order of bnb 0x2b22eab7… → eth 0x9254eb94… (a real deBridge order).
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


link : String -> String -> Maybe String -> Maybe ( String, String )
link protocol sourceLeg transferId =
    BridgeExplorer.explorerLink { protocol = protocol, sourceLeg = sourceLeg, transferId = transferId }
        |> Maybe.map (\l -> ( l.url, l.label ))


suite : Test
suite =
    describe "Util.BridgeExplorer"
        [ test "an unknown name is shown as the API spells it, with a capital first letter" <|
            -- the API can name protocols the dashboard has no display name
            -- for yet (across, relay, wormhole); "across" must not read as a word
            \_ ->
                ( [ "across", "relay", "wormhole", "Mayan", "", " across ", " CCTP ", " LayerZERO ", "  " ] |> List.map BridgeExplorer.protocolName
                , [ "oft-x", "", " UsDt0 " ] |> List.map BridgeExplorer.appName
                )
                    |> Expect.equal
                        ( [ "Across", "Relay", "Wormhole", "Mayan", "", "Across", "CCTP", "LayerZero", "" ]
                        , [ "Oft-x", "", "USDT0" ]
                        )
        , test "each explorer's URL" <|
            \_ ->
                [ link "layerzero" usdt0Send Nothing
                , link " LayerZERO " usdt0Send Nothing

                -- deExplorer opens the order by its id (app.debridge.finance only redirects there)
                , link "debridge" usdt0Send (Just debridgeOrder)

                -- thorchain.net takes the inbound hash in capitals, without 0x
                , link "thorchain" thorchainDeposit Nothing

                -- Range finds a CCTP transfer by its source tx, as lowercase hex
                -- after 0x; without the 0x Range answers "No results found"
                -- (checked 2026-11-09)
                , link "cctp" (String.toUpper cctpBurn) (Just "0x33e1")
                ]
                    |> Expect.equal
                        [ Just ( "https://layerzeroscan.com/tx/0x507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0", "LayerZero Scan" )
                        , Just ( "https://layerzeroscan.com/tx/0x507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0", "LayerZero Scan" )
                        , Just ( "https://app.debridge.com/order?orderId=" ++ debridgeOrder, "deExplorer" )
                        , Just ( "https://thorchain.net/tx/42D529A72CECD6ECE546D5AC0D2A6C2A9407876B66478A33917D8928833433F8", "THORChain Explorer" )
                        , Just ( "https://usdc.range.org/transactions?s=0xe8c9cfb2ee4a139c45173e27784b669e9741fb04418475cd7f8dfbf0124202af", "Range CCTP Explorer" )
                        ]
        , test "a malformed id or an unknown protocol never becomes a URL" <|
            \_ ->
                [ ( "deBridge without an order id", link "debridge" usdt0Send Nothing )
                , ( "a transfer id that is not an order id", link "debridge" usdt0Send (Just "0x12") )
                , ( "an order id with a query appended", link "debridge" usdt0Send (Just (debridgeOrder ++ "&x=1")) )
                , ( "a script as the order id", link "debridge" usdt0Send (Just "javascript:alert(1)") )
                , ( "a source leg that is not a tx hash", link "layerzero" "/../evil_T1" Nothing )
                , ( "a CCTP source leg that is not a tx hash", link "cctp" "abc&s=x_T1" Nothing )
                , ( "a truncated hash", link "cctp" (String.dropLeft 1 cctpBurn) Nothing )
                , ( "an unknown protocol", link "symbiosis" usdt0Send Nothing )
                , ( "wormhole", link "wormhole" usdt0Send Nothing )
                ]
                    |> List.filter (Tuple.second >> (/=) Nothing)
                    |> Expect.equal []
        ]
