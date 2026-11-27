module Model.Pathfinder.SelectionStatsTest exposing (suite)

import Animation
import Api.Data
import Dict
import Expect
import Fixtures.Api
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Json.Decode
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Network exposing (Network)
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..))
import Model.Pathfinder.SelectionStats as SelectionStats
import Model.Pathfinder.Tx as Tx exposing (Tx)
import RemoteData
import Test exposing (Test, describe, test)


addr : String -> Id
addr =
    Id.init "eth"


usd : Float -> Api.Data.Values
usd v =
    { value = 1, fiatValues = [ { code = "usd", value = v } ] }


accountTx : String -> String -> String -> Float -> Tx
accountTx id from to v =
    { id = Id.init "eth" id
    , hovered = False
    , selected = False
    , type_ =
        Tx.Account
            { from = addr from
            , to = addr to
            , fromAddress = Nothing
            , toAddress = Nothing
            , value = usd v
            , raw =
                { contractCreation = Nothing
                , currency = "eth"
                , fee = Nothing
                , fromAddress = from
                , height = 1
                , identifier = id
                , isExternal = Nothing
                , network = "eth"
                , timestamp = 0
                , toAddress = to
                , tokenTxId = Nothing
                , txHash = id
                , txType = "account"
                , value = usd v
                }
            }
    , x = 0
    , y = Animation.static 0
    , dx = 0
    , dy = 0
    , clock = 0
    , opacity = Animation.static 1
    , isStartingPoint = False
    , conversionType = Nothing
    , unsupportedConversions = []
    , index = 0
    }


{-| o --100--> a --30--> b --5--> o2, and a --7--> b again.
-}
network : Network
network =
    let
        base =
            Network.init
    in
    { base
        | addresses =
            [ "o", "a", "b", "o2" ]
                |> List.map (\a -> ( addr a, Address.init (addr a) { x = 0, y = 0 } ))
                |> Dict.fromList
        , txs =
            [ accountTx "t1" "o" "a" 100, accountTx "t2" "a" "b" 30, accountTx "t3" "b" "o2" 5, accountTx "t4" "a" "b" 7 ]
                |> List.map (\t -> ( t.id, t ))
                |> Dict.fromList
    }


stats : List MultiSelectOptions -> SelectionStats.Stats
stats sel =
    SelectionStats.compute "usd" sel network


ab : List MultiSelectOptions
ab =
    [ MSelectedAddress (addr "a"), MSelectedAddress (addr "b") ]


suite : Test
suite =
    describe "Model.Pathfinder.SelectionStats"
        [ test "flows into, out of and within the selection" <|
            \_ ->
                stats ab
                    |> (\s -> ( s.inflow, s.outflow, s.between ))
                    |> Expect.equal ( 100, 5, 37 )
        , test "counts addresses and transactions separately" <|
            \_ ->
                stats (MSelectedTx (Id.init "eth" "t1") :: ab)
                    |> (\s -> ( s.addresses, s.txs ))
                    |> Expect.equal ( 2, 1 )
        , test "two selected addresses: the transactions between them" <|
            \_ -> stats ab |> .txsBetween |> Expect.equal (Just 2)
        , test "not for three" <|
            \_ -> stats (MSelectedAddress (addr "o") :: ab) |> .txsBetween |> Expect.equal Nothing
        , test "a transfer without a fiat value in the currency counts as 0" <|
            \_ ->
                SelectionStats.compute "eur" ab network
                    |> .inflow
                    |> Expect.equal 0
        , test "totals and activity come from the addresses' own data" <|
            \_ ->
                case Json.Decode.decodeString Api.Data.addressDecoder Fixtures.Api.address of
                    Ok data ->
                        SelectionStats.compute "usd"
                            ab
                            { network | addresses = Dict.map (\_ a -> { a | data = RemoteData.Success data }) network.addresses }
                            |> (\s -> ( ( s.received, s.sent, s.balance ), ( s.firstActivity, s.clusters ) ))
                            -- the spec example: 20 usd each, first tx at 123456789, one cluster
                            |> Expect.equal ( ( 40, 40, 40 ), ( Just 123456789, 1 ) )

                    Err e ->
                        Expect.fail (Json.Decode.errorToString e)
        ]
