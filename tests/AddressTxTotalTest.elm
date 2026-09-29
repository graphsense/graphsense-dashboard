module AddressTxTotalTest exposing (suite)

{-| The transaction total in the address panel: exact parts add up, a floored
part turns the total into the larger part, itself a floor.
-}

import Api.Data
import Data.Api
import Dict
import Expect
import Model.Pathfinder.Address exposing (getTxTotal, isFloored)
import Test exposing (Test, describe, test)


apiAddress : Int -> Int -> List String -> Api.Data.Address
apiAddress incoming outgoing flooredFields =
    { actors = Nothing
    , address = "addr"
    , balance = Data.Api.values
    , currency = "eth"
    , cluster = 1
    , freshClusterId = Nothing
    , firstTx = Nothing
    , inDegree = 1
    , isContract = Nothing
    , lastTx = Nothing
    , noIncomingTxs = incoming
    , noOutgoingTxs = outgoing
    , outDegree = 1
    , status = Api.Data.AddressStatusClean
    , tokenBalances = Nothing
    , totalReceived = Data.Api.values
    , totalSpent = Data.Api.values
    , totalTokensReceived = Nothing
    , totalTokensSpent = Nothing
    , isPossibleService = Nothing
    , qualifiers =
        if List.isEmpty flooredFields then
            Nothing

        else
            flooredFields |> List.map (\f -> ( f, "gt" )) |> Dict.fromList |> Just
    }


suite : Test
suite =
    describe "Model.Pathfinder.Address.getTxTotal"
        [ test "exact counts add up" <|
            \_ ->
                getTxTotal (apiAddress 300 400 [])
                    |> Expect.equal ( 700, False )
        , test "a floored incoming count makes the total the larger part, floored" <|
            \_ ->
                getTxTotal (apiAddress 500 217 [ "no_incoming_txs" ])
                    |> Expect.equal ( 500, True )
        , test "a floored outgoing count that is the smaller part still floors the total" <|
            \_ ->
                getTxTotal (apiAddress 800 500 [ "no_outgoing_txs" ])
                    |> Expect.equal ( 800, True )
        , test "both floored" <|
            \_ ->
                getTxTotal (apiAddress 500 500 [ "no_incoming_txs", "no_outgoing_txs" ])
                    |> Expect.equal ( 500, True )
        , test "a qualifier other than gt is not a floor" <|
            \_ ->
                isFloored "no_incoming_txs" { emptyQualified | qualifiers = Just (Dict.fromList [ ( "no_incoming_txs", "eq" ) ]) }
                    |> Expect.equal False
        ]


emptyQualified : Api.Data.Address
emptyQualified =
    apiAddress 1 1 []
