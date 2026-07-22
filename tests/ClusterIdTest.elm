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
    , aggregatesTruncated = Nothing
    , cutoffFloorFields = Nothing
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
        ]
