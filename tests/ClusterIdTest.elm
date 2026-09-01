module ClusterIdTest exposing (suite)

import Api.Data
import Data.Api
import Data.Pathfinder.Address exposing (address1)
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Network as Network
import Dict
import Expect
import Init.Pathfinder.Id as PathfinderId
import Model.Pathfinder.Address exposing (getClusterId)
import Model.Pathfinder.Network exposing (Network, getAddressIdsInCluster, isClusterFriendAlreadyOnGraph)
import RecordSetter exposing (s_addresses, s_data)
import RemoteData
import Test exposing (Test, describe, test)
import Util.Data


apiAddress : Maybe Int -> Api.Data.Address
apiAddress freshClusterId =
    { actors = Nothing
    , address = "a1234567"
    , balance = Data.Api.values
    , currency = "btc"
    , cluster = 1
    , freshClusterId = freshClusterId
    , firstTx = { height = 1, timestamp = 0, txHash = "h" }
    , inDegree = 1
    , isContract = Nothing
    , lastTx = { height = 1, timestamp = 0, txHash = "h" }
    , noIncomingTxs = 1
    , noOutgoingTxs = 1
    , outDegree = 1
    , status = Api.Data.AddressStatusClean
    , tokenBalances = Nothing
    , totalReceived = Data.Api.values
    , totalSpent = Data.Api.values
    , totalTokensReceived = Nothing
    , totalTokensSpent = Nothing
    , isPossibleService = Nothing
    , qualifiers = Nothing
    }


networkWith : Api.Data.Address -> Network
networkWith api =
    Network.oneAddress
        |> s_addresses
            (Dict.singleton Id.address1
                (address1 |> s_data (RemoteData.Success api))
            )


suite : Test
suite =
    describe "fresh-aware cluster id derivation"
        [ describe "Util.Data.addressCluster"
            [ test "prefers freshClusterId when present" <|
                \_ ->
                    Util.Data.addressCluster (apiAddress (Just 42))
                        |> Expect.equal 42
            , test "falls back to legacy cluster" <|
                \_ ->
                    Util.Data.addressCluster (apiAddress Nothing)
                        |> Expect.equal 1
            ]
        , describe "Init.Pathfinder.Id.initClusterIdFromAddress"
            [ test "derives the fresh cluster id when present" <|
                \_ ->
                    PathfinderId.initClusterIdFromAddress (apiAddress (Just 42))
                        |> Expect.equal (PathfinderId.initClusterId "btc" 42)
            , test "falls back to the legacy cluster id" <|
                \_ ->
                    PathfinderId.initClusterIdFromAddress (apiAddress Nothing)
                        |> Expect.equal (PathfinderId.initClusterId "btc" 1)
            ]
        , describe "Model.Pathfinder.Address.getClusterId"
            [ test "derives the fresh cluster id when present" <|
                \_ ->
                    getClusterId (address1 |> s_data (RemoteData.Success (apiAddress (Just 42))))
                        |> Expect.equal (Just (PathfinderId.initClusterId "btc" 42))
            , test "agrees with initClusterIdFromAddress" <|
                \_ ->
                    getClusterId (address1 |> s_data (RemoteData.Success (apiAddress (Just 42))))
                        |> Expect.equal (Just (PathfinderId.initClusterIdFromAddress (apiAddress (Just 42))))
            ]
        , describe "Model.Pathfinder.Network cluster membership"
            [ test "isClusterFriendAlreadyOnGraph matches the fresh-derived id" <|
                \_ ->
                    networkWith (apiAddress (Just 42))
                        |> isClusterFriendAlreadyOnGraph (PathfinderId.initClusterId "btc" 42)
                        |> Expect.equal True
            , test "isClusterFriendAlreadyOnGraph does not match the legacy id when a fresh one is set" <|
                \_ ->
                    networkWith (apiAddress (Just 42))
                        |> isClusterFriendAlreadyOnGraph (PathfinderId.initClusterId "btc" 1)
                        |> Expect.equal False
            , test "isClusterFriendAlreadyOnGraph matches the legacy id without a fresh one" <|
                \_ ->
                    networkWith (apiAddress Nothing)
                        |> isClusterFriendAlreadyOnGraph (PathfinderId.initClusterId "btc" 1)
                        |> Expect.equal True
            , test "getAddressIdsInCluster finds addresses by the fresh-derived id" <|
                \_ ->
                    networkWith (apiAddress (Just 42))
                        |> getAddressIdsInCluster (PathfinderId.initClusterId "btc" 42)
                        |> Expect.equal [ Id.address1 ]
            , test "getAddressIdsInCluster finds nothing under the legacy id when a fresh one is set" <|
                \_ ->
                    networkWith (apiAddress (Just 42))
                        |> getAddressIdsInCluster (PathfinderId.initClusterId "btc" 1)
                        |> Expect.equal []
            , test "ring membership helpers agree with getClusterId" <|
                \_ ->
                    let
                        net =
                            networkWith (apiAddress (Just 42))
                    in
                    net.addresses
                        |> Dict.get Id.address1
                        |> Maybe.andThen getClusterId
                        |> Maybe.map (\cid -> isClusterFriendAlreadyOnGraph cid net)
                        |> Expect.equal (Just True)
            ]
        , describe "Util.Data.selfCluster"
            -- on lite networks core synthesizes the cluster from the address
            -- itself instead of fetching it (account-model clusters are
            -- singletons); these pin the eth wire shape of that derivation
            [ test "roots at the address with a single member" <|
                \_ ->
                    Util.Data.selfCluster (apiAddress (Just 42))
                        |> Expect.all
                            [ .rootAddress >> Expect.equal "a1234567"
                            , .noAddresses >> Expect.equal 1
                            , .currency >> Expect.equal "btc"
                            , .noAddressTags >> Expect.equal 0
                            , .bestAddressTag >> Expect.equal Nothing
                            ]
            , test "derives the entity id fresh-aware" <|
                \_ ->
                    Util.Data.selfCluster (apiAddress (Just 42))
                        |> .cluster
                        |> Expect.equal 42
            , test "falls back to the legacy cluster id" <|
                \_ ->
                    Util.Data.selfCluster (apiAddress Nothing)
                        |> .cluster
                        |> Expect.equal 1
            , test "mirrors the address stats" <|
                \_ ->
                    let
                        a =
                            apiAddress Nothing
                    in
                    Util.Data.selfCluster a
                        |> Expect.all
                            [ .balance >> Expect.equal a.balance
                            , .totalReceived >> Expect.equal a.totalReceived
                            , .totalSpent >> Expect.equal a.totalSpent
                            , .firstTx >> Expect.equal a.firstTx
                            , .lastTx >> Expect.equal a.lastTx
                            , .inDegree >> Expect.equal a.inDegree
                            , .outDegree >> Expect.equal a.outDegree
                            , .noIncomingTxs >> Expect.equal a.noIncomingTxs
                            , .noOutgoingTxs >> Expect.equal a.noOutgoingTxs
                            , .tokenBalances >> Expect.equal a.tokenBalances
                            ]
            , test "cluster id agrees with initClusterIdFromAddress" <|
                \_ ->
                    let
                        a =
                            apiAddress (Just 42)

                        c =
                            Util.Data.selfCluster a
                    in
                    PathfinderId.initClusterId c.currency c.cluster
                        |> Expect.equal (PathfinderId.initClusterIdFromAddress a)
            ]
        , describe "initClusterId hex encoding"
            -- Hex.toString hard-freezes on ints >= 2^35 (its `//` recursion
            -- wraps to 32-bit signed), so initClusterId uses its own encoder;
            -- these pin format parity for small ids and correct, terminating
            -- output across the whole exact-Int range.
            [ test "matches the legacy Hex.toString format for small ids" <|
                \_ ->
                    PathfinderId.initClusterId "btc" 264711
                        |> Expect.equal ( "btc", "40a07" )
            , test "zero" <|
                \_ ->
                    PathfinderId.initClusterId "btc" 0
                        |> Expect.equal ( "btc", "0" )
            , test "terminates and encodes a 48-bit server-minted id" <|
                \_ ->
                    PathfinderId.initClusterId "arb" 246237048184833
                        |> Expect.equal ( "arb", "dff387c9a001" )
            , test "handles the largest exact Int (2^53 - 1)" <|
                \_ ->
                    PathfinderId.initClusterId "eth" 9007199254740991
                        |> Expect.equal ( "eth", "1fffffffffffff" )
            ]
        ]
