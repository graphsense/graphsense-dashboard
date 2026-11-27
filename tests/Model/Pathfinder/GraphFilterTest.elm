module Model.Pathfinder.GraphFilterTest exposing (suite)

import Api.Data
import Config.Pathfinder exposing (TracingMode(..))
import Dict
import Expect
import Fixtures.Api
import Init.Pathfinder.Address as Address
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.GraphFilter as GraphFilter exposing (Config, Model)
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Tx as Tx exposing (Io, Tx)
import RecordSetter exposing (s_isStartingPoint)
import RemoteData
import Set
import Support.GraphFilterFixture exposing (accountTx, addr, baseTx, chain, day, networkOf, txId, usd)
import Support.SwapFixture exposing (dexSwap)
import Test exposing (Test, describe, test)
import Time



-- FIXTURES


{-| A UTXO tx; each io is (address, value, on the graph?).
-}
utxoTx : String -> List ( String, Float, Bool ) -> List ( String, Float, Bool ) -> Float -> Tx
utxoTx id inputs outputs totalOutput =
    let
        io ( a, v, shown ) =
            ( Id.init "btc" a
            , Io (usd v)
                (if shown then
                    Just (Address.init (Id.init "btc" a) { x = 0, y = 0 })

                 else
                    Nothing
                )
                1
            )

        raw =
            { coinbase = False
            , currency = "btc"
            , height = 1
            , inputs = Nothing
            , noInputs = List.length inputs
            , noOutputs = List.length outputs
            , outputs = Nothing
            , timestamp = 100
            , totalInput = usd totalOutput
            , totalOutput = usd totalOutput
            , txHash = id
            , txType = "utxo"
            , heuristics = Nothing
            }
    in
    baseTx (Id.init "btc" id)
        (Tx.Utxo
            { inputs = Dict.fromList (List.map io inputs)
            , outputs = Dict.fromList (List.map io outputs)
            , raw = raw
            }
        )


txConfig : Config
txConfig =
    { fiatCode = "usd", tracingMode = TransactionTracingMode, protected = Set.empty }


tiny : Model
tiny =
    { init | hideTiny = True }


init : Model
init =
    GraphFilter.init


period : Int -> Int -> Maybe GraphFilter.Period
period from until =
    Just
        { from = Time.millisToPosix (from * 1000)
        , until = Time.millisToPosix (until * 1000)
        }


hiddenTxs : Config -> Model -> Network -> List Id
hiddenTxs c m =
    GraphFilter.apply c m >> .hiddenTxs >> Set.toList



-- TESTS


suite : Test
suite =
    describe "Model.Pathfinder.GraphFilter"
        [ describe "inactive"
            [ test "the initial model has no active filter" <|
                \_ -> GraphFilter.activeCount init |> Expect.equal 0
            , test "an inactive filter hides nothing" <|
                \_ -> GraphFilter.apply txConfig init chain |> Expect.equal GraphFilter.emptyResult
            , test "a changed limit alone does not activate the filter" <|
                \_ -> GraphFilter.isActive { init | limit = 500 } |> Expect.equal False
            , test "each kind of filter counts once" <|
                \_ ->
                    GraphFilter.activeCount
                        { tiny | period = period 0 1, excludedAssets = Set.fromList [ "eth", "usdt" ] }
                        |> Expect.equal 3
            ]
        , describe "value"
            [ test "hides a transfer under the limit and keeps the one above" <|
                \_ -> hiddenTxs txConfig tiny chain |> Expect.equal [ txId "t2" ]
            , test "the limit is inclusive" <|
                \_ -> hiddenTxs txConfig { tiny | limit = 5 } chain |> Expect.equal []
            , test "a transfer without a fiat value in the chosen currency stays" <|
                \_ -> hiddenTxs { txConfig | fiatCode = "eur" } tiny chain |> Expect.equal []
            ]
        , describe "UTXO value: the outputs shown on the graph (2026-11-27 D3)"
            [ test "an off-graph output does not count" <|
                \_ ->
                    networkOf []
                        [ utxoTx "u1" [ ( "in", 1005, False ) ] [ ( "shown", 5, True ), ( "other", 1000, False ) ] 1005 ]
                        |> hiddenTxs txConfig tiny
                        |> Expect.equal [ Id.init "btc" "u1" ]
            , test "a shown output decides, not a larger shown input" <|
                \_ ->
                    networkOf []
                        [ utxoTx "u1" [ ( "in", 50, True ) ] [ ( "shown", 5, True ) ] 55 ]
                        |> hiddenTxs txConfig tiny
                        |> Expect.equal [ Id.init "btc" "u1" ]
            , test "the largest shown output decides" <|
                \_ ->
                    utxoTx "u1" [ ( "in", 900, True ) ] [ ( "a", 5, True ), ( "b", 40, True ), ( "c", 800, False ) ] 845
                        |> GraphFilter.txFiatValue "usd"
                        |> Expect.equal (Just 40)
            , test "with no output shown, the shown inputs add up" <|
                \_ ->
                    utxoTx "u1" [ ( "i1", 6, True ), ( "i2", 7, True ), ( "i3", 500, False ) ] [ ( "out", 500, False ) ] 500
                        |> GraphFilter.txFiatValue "usd"
                        |> Expect.equal (Just 13)
            , test "without any shown io, the total output decides" <|
                \_ ->
                    utxoTx "u1" [ ( "in", 1, False ) ] [ ( "out", 1, False ) ] 3
                        |> GraphFilter.txFiatValue "usd"
                        |> Expect.equal (Just 3)
            ]
        , describe "time"
            [ test "hides transfers outside the period" <|
                \_ ->
                    hiddenTxs txConfig { init | period = period (day 15) (day 25) } chain
                        |> Expect.equal [ txId "t1" ]
            , test "both ends of the period are inclusive" <|
                \_ ->
                    hiddenTxs txConfig { init | period = period (day 10) (day 20) } chain
                        |> Expect.equal []
            ]
        , describe "time helpers"
            [ test "a day runs from midnight to the last millisecond" <|
                \_ ->
                    GraphFilter.dayBounds Time.utc (Time.millisToPosix ((day 10 + 3600 * 12) * 1000))
                        |> Expect.equal
                            { from = Time.millisToPosix (day 10 * 1000)
                            , until = Time.millisToPosix (day 11 * 1000 - 1)
                            }
            , test "a date field value is the start of that day" <|
                \_ ->
                    GraphFilter.fromDateString Time.utc "1970-01-11"
                        |> Expect.equal (Just (Time.millisToPosix (day 10 * 1000)))
            , test "a date field value round-trips" <|
                \_ ->
                    GraphFilter.fromDateString Time.utc "2026-03-02"
                        |> Maybe.map (GraphFilter.toDateString Time.utc)
                        |> Expect.equal (Just "2026-03-02")
            , test "an unparsable date is no date" <|
                \_ -> GraphFilter.fromDateString Time.utc "2026-13-45" |> Expect.equal Nothing
            ]
        , describe "activity strip"
            [ test "one bucket per day from the first transfer to the last, money per day" <|
                \_ ->
                    GraphFilter.activity "usd" Time.utc 60 TransactionTracingMode init chain
                        |> (\buckets -> ( List.length buckets, List.map .value buckets |> List.filter ((<) 0) ))
                        |> Expect.equal ( 11, [ 1000, 5 ] )
            , test "a long span gets several days per bucket" <|
                \_ ->
                    -- daily transfers for 30 days: no empty stretch to leave out
                    networkOf [ "a", "b" ] (List.range 0 29 |> List.map (\i -> accountTx ("d" ++ String.fromInt i) "a" "b" "eth" (usd 1) (day i)))
                        |> GraphFilter.activity "usd" Time.utc 10 TransactionTracingMode init
                        |> (\buckets -> ( List.length buckets, List.map .value buckets |> List.sum, List.filterMap .gapBefore buckets ))
                        |> Expect.equal ( 10, 30, [] )
            , test "a long empty stretch is left out instead of squeezing the bars" <|
                \_ ->
                    -- two bursts of activity, 5 years apart
                    networkOf [ "a", "b" ]
                        [ accountTx "x1" "a" "b" "eth" (usd 10) (day 0)
                        , accountTx "x2" "a" "b" "eth" (usd 20) (day 1)
                        , accountTx "y1" "a" "b" "eth" (usd 30) (day 1826)
                        , accountTx "y2" "a" "b" "eth" (usd 40) (day 1828)
                        ]
                        |> GraphFilter.activity "usd" Time.utc 60 TransactionTracingMode init
                        |> List.map (\b -> ( b.value, b.gapBefore ))
                        |> Expect.equal [ ( 10, Nothing ), ( 20, Nothing ), ( 30, Just 1824 ), ( 0, Nothing ), ( 40, Nothing ) ]
            , test "the bars keep their real dates after a gap" <|
                \_ ->
                    networkOf [ "a", "b" ]
                        [ accountTx "x1" "a" "b" "eth" (usd 10) (day 0)
                        , accountTx "y1" "a" "b" "eth" (usd 30) (day 1826)
                        ]
                        |> GraphFilter.activity "usd" Time.utc 60 TransactionTracingMode init
                        |> List.map (.period >> .from >> Time.posixToMillis)
                        |> Expect.equal [ 0, day 1826 * 1000 ]
            , test "with few bars to spare, a shorter empty stretch is left out too" <|
                \_ ->
                    GraphFilter.activity "usd" Time.utc 5 TransactionTracingMode init chain
                        |> (\buckets -> ( List.length buckets, List.map .value buckets |> List.sum, List.filterMap .gapBefore buckets ))
                        |> Expect.equal ( 2, 1005, [ 9 ] )
            , test "transfers the other filters hide do not count" <|
                \_ ->
                    GraphFilter.activity "usd" Time.utc 60 TransactionTracingMode { init | excludedAssets = Set.singleton "usdt" } chain
                        |> List.map .value
                        |> Expect.equal [ 5 ]
            , test "the time filter itself does not shrink the strip" <|
                \_ ->
                    GraphFilter.activity "usd" Time.utc 60 TransactionTracingMode { init | period = period (day 15) (day 25) } chain
                        |> List.length
                        |> Expect.equal 11
            , test "the aggregated mode has no strip" <|
                \_ ->
                    GraphFilter.activity "usd" Time.utc 60 AggregateTracingMode init chain
                        |> Expect.equal []
            ]
        , describe "inputs (what the result depends on)"
            [ test "moving an address does not change them" <|
                \_ ->
                    GraphFilter.inputs txConfig tiny { chain | addresses = Dict.map (\_ a -> { a | x = a.x + 5 }) chain.addresses }
                        |> Expect.equal (GraphFilter.inputs txConfig tiny chain)
            , test "a new transaction changes them" <|
                \_ ->
                    GraphFilter.inputs txConfig tiny { chain | txs = Dict.insert (txId "t9") (accountTx "t9" "c" "d" "eth" (usd 1) 0) chain.txs }
                        |> Expect.notEqual (GraphFilter.inputs txConfig tiny chain)
            , test "a newly protected address changes them" <|
                \_ ->
                    GraphFilter.inputs { txConfig | protected = Set.singleton (addr "c") } tiny chain
                        |> Expect.notEqual (GraphFilter.inputs txConfig tiny chain)
            ]
        , describe "assets"
            [ test "hides transfers of an excluded asset" <|
                \_ ->
                    hiddenTxs txConfig { init | excludedAssets = Set.singleton "usdt" } chain
                        |> Expect.equal [ txId "t1" ]
            , test "asset names compare case-insensitively" <|
                \_ ->
                    networkOf [ "a", "b" ] [ accountTx "t1" "a" "b" "USDT" (usd 1) 0 ]
                        |> hiddenTxs txConfig { init | excludedAssets = Set.singleton "usdt" }
                        |> Expect.equal [ txId "t1" ]
            , test "both legs of a swap stay when either asset is included" <|
                \_ ->
                    swapNetwork
                        |> hiddenTxs txConfig { init | excludedAssets = Set.singleton "usdc" }
                        |> Expect.equal []
            , test "a swap goes when both assets are excluded, with its edge" <|
                \_ ->
                    swapNetwork
                        |> GraphFilter.apply txConfig { init | excludedAssets = Set.fromList [ "usdc", "usdt" ] }
                        |> (\r -> ( Set.toList r.hiddenTxs, Set.toList r.hiddenConversions ))
                        |> Expect.equal ( [ txId "leg-in", txId "leg-out" ], [ ( txId "leg-in", txId "leg-out" ) ] )
            , test "lists the assets on the graph once each, in lower case" <|
                \_ ->
                    networkOf [] [ accountTx "t1" "a" "b" "USDT" (usd 1) 0, accountTx "t2" "a" "b" "usdt" (usd 1) 0, accountTx "t3" "a" "b" "eth" (usd 1) 0 ]
                        |> GraphFilter.assetsOnGraph TransactionTracingMode
                        |> Expect.equal [ "eth", "usdt" ]
            ]
        , describe "addresses"
            [ test "an address whose only transfers are hidden goes too" <|
                \_ ->
                    GraphFilter.apply txConfig tiny chain
                        |> .hiddenAddresses
                        |> Set.toList
                        |> Expect.equal [ addr "c" ]
            , test "an address without transfers on the graph is never touched" <|
                \_ ->
                    GraphFilter.apply txConfig { init | excludedAssets = Set.fromList [ "eth", "usdt" ] } chain
                        |> .hiddenAddresses
                        |> Set.member (addr "d")
                        |> Expect.equal False
            , test "a protected address is greyed, not hidden" <|
                \_ ->
                    GraphFilter.apply { txConfig | protected = Set.singleton (addr "c") } tiny chain
                        |> (\r -> ( Set.toList r.hiddenAddresses, Set.toList r.greyedAddresses ))
                        |> Expect.equal ( [], [ addr "c" ] )
            , test "a starting point is greyed, not hidden" <|
                \_ ->
                    { chain | addresses = Dict.update (addr "c") (Maybe.map (s_isStartingPoint True)) chain.addresses }
                        |> GraphFilter.apply txConfig tiny
                        |> (\r -> ( Set.toList r.hiddenAddresses, Set.toList r.greyedAddresses ))
                        |> Expect.equal ( [], [ addr "c" ] )
            ]
        , describe "aggregated tracing mode"
            [ test "hides an edge whose total is under the limit, and its lone address" <|
                \_ ->
                    aggNetwork
                        |> GraphFilter.apply aggConfig tiny
                        |> (\r -> ( Set.toList r.hiddenAggEdges, Set.toList r.hiddenAddresses ))
                        |> Expect.equal ( [ ( addr "a", addr "c" ) ], [ addr "c" ] )
            , test "both directions add up" <|
                \_ ->
                    aggNetwork
                        |> GraphFilter.apply aggConfig { tiny | limit = 8 }
                        |> .hiddenAggEdges
                        |> Expect.equal Set.empty
            , test "the time filter does not apply" <|
                \_ ->
                    aggNetwork
                        |> GraphFilter.apply aggConfig { init | period = period 0 1 }
                        |> .hiddenAggEdges
                        |> Expect.equal Set.empty
            , test "an edge of an excluded asset goes" <|
                \_ ->
                    aggNetwork
                        |> GraphFilter.apply aggConfig { init | excludedAssets = Set.singleton "eth" }
                        |> .hiddenAggEdges
                        |> Set.size
                        |> Expect.equal 2
            ]
        ]


swapNetwork : Network
swapNetwork =
    let
        legIn =
            accountTx "leg-in" "a" "router" "usdt" (usd 100) 0

        legOut =
            accountTx "leg-out" "router" "a" "usdc" (usd 100) 0

        base =
            networkOf [ "a", "router" ] [ legIn, legOut ]
    in
    { base
        | conversions =
            Dict.singleton ( legIn.id, legOut.id )
                (ConversionEdge.init dexSwap
                    ( legIn.id, legOut.id )
                    ( addr "a", addr "a" )
                    (Tx.getRawTx legIn)
                    (Tx.getRawTx legOut)
                )
    }


aggConfig : Config
aggConfig =
    { txConfig | tracingMode = AggregateTracingMode }


{-| a <-> b: 1000 $ one way; a <-> c: 5 $ + 4 $.
-}
aggNetwork : Network
aggNetwork =
    let
        base =
            networkOf [ "a", "b", "c" ] []
    in
    { base
        | aggEdges =
            Dict.fromList
                [ ( ( addr "a", addr "b" ), aggEdge "a" "b" (Just 1000) Nothing )
                , ( ( addr "a", addr "c" ), aggEdge "a" "c" (Just 5) (Just 4) )
                ]
    }


aggEdge : String -> String -> Maybe Float -> Maybe Float -> AggEdge
aggEdge a b a2b b2a =
    let
        -- the spec example, with our value; a decode failure leaves the
        -- edge without values, which the expectations below would catch
        toNeighbor v =
            v
                |> Maybe.map
                    (\f ->
                        Json.Decode.decodeString Api.Data.neighborAddressDecoder Fixtures.Api.neighborAddress
                            |> Result.map (\n -> Just { n | value = usd f, tokenValues = Nothing })
                            |> Result.withDefault Nothing
                    )
                |> Maybe.withDefault Nothing
                |> RemoteData.Success
    in
    { a = addr a
    , b = addr b
    , aAddress = Nothing
    , bAddress = Nothing
    , a2b = toNeighbor a2b
    , b2a = toNeighbor b2a
    , txs = Set.empty
    , selected = False
    , hovered = False
    , labelOffset = Nothing
    }
