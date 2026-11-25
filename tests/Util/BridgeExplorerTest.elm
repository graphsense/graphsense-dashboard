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


{-| The origin tx of an Across transfer, a Relay deposit and its request id,
and a Wormhole VAA id (`<chain>/<emitter>/<sequence>`), from the adapter's
recorded transfers.
-}
acrossDeposit : String
acrossDeposit =
    "3376c148afedae87af4cbe699cefc42ae3d9371347f879b25e06a4cdbdf795a3_T367"


relayDeposit : String
relayDeposit =
    "79c7f1ee54d0736783955845f829606b8ef0852e887e811ece4ed8994f966391_T24"


relayOrder : String
relayOrder =
    "0x21eb06dc1d3c35a302e1cc3ed83d9a35de60d017f0a3c44abfe5a1114e3688a6"


wormholeVaa : String
wormholeVaa =
    "2/0000000000000000000000003ee18b2214aff97000d974cf647e7c347e8fa585/704561"


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

                -- Across opens a transfer by its origin tx, Relay a request by
                -- its deposit tx, Wormholescan a VAA by its id from either end
                -- (checked 2026-11-17)
                , link "across" acrossDeposit (Just "1108583")
                , link "relay" relayDeposit (Just relayOrder)
                , link "wormhole" acrossDeposit (Just wormholeVaa)
                , link "wormhole" "unknown" (Just wormholeVaa)
                ]
                    |> Expect.equal
                        [ Just ( "https://layerzeroscan.com/tx/0x507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0", "LayerZero Scan" )
                        , Just ( "https://layerzeroscan.com/tx/0x507fa26239c34aefcefcb660457a73065383f284ca4673e436ea168e5519b2b0", "LayerZero Scan" )
                        , Just ( "https://app.debridge.com/order?orderId=" ++ debridgeOrder, "deExplorer" )
                        , Just ( "https://thorchain.net/tx/42D529A72CECD6ECE546D5AC0D2A6C2A9407876B66478A33917D8928833433F8", "THORChain Explorer" )
                        , Just ( "https://usdc.range.org/transactions?s=0xe8c9cfb2ee4a139c45173e27784b669e9741fb04418475cd7f8dfbf0124202af", "Range CCTP Explorer" )
                        , Just ( "https://across.to/transfer/0x3376c148afedae87af4cbe699cefc42ae3d9371347f879b25e06a4cdbdf795a3", "Across Explorer" )
                        , Just ( "https://relay.link/transaction/0x79c7f1ee54d0736783955845f829606b8ef0852e887e811ece4ed8994f966391", "Relay Explorer" )
                        , Just ( "https://wormholescan.io/#/tx/2/0000000000000000000000003ee18b2214aff97000d974cf647e7c347e8fa585/704561", "Wormholescan" )
                        , Just ( "https://wormholescan.io/#/tx/2/0000000000000000000000003ee18b2214aff97000d974cf647e7c347e8fa585/704561", "Wormholescan" )
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
                , ( "wormhole without a VAA id", link "wormhole" usdt0Send Nothing )
                ]
                    ++ List.concatMap
                        (\leg ->
                            [ ( "an Across half row without its source leg", link "across" leg (Just "1") )
                            , ( "a Relay half row without its source leg", link "relay" leg (Just relayOrder) )
                            ]
                        )
                        [ "unknown", "pending", "", "abc" ]
                    ++ List.map (\id -> ( "a VAA id of another shape", link "wormhole" usdt0Send (Just id) ))
                        [ "2/0x3ee18b22/704561"
                        , "2/" ++ String.repeat 63 "0" ++ "/1"
                        , "x/" ++ String.repeat 64 "0" ++ "/1"
                        , "2/" ++ String.repeat 64 "0" ++ "/1/9"
                        , "2/" ++ String.repeat 64 "0" ++ "/1?a=b"
                        , "2/" ++ String.repeat 64 "0" ++ "/"
                        ]
                    |> List.filter (Tuple.second >> (/=) Nothing)
                    |> Expect.equal []
        ]
