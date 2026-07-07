module AddressSharedServiceTest exposing (suite)

import Api.Data
import Data.Api
import Data.Pathfinder.Address exposing (address1)
import Expect
import Model.Pathfinder.Address exposing (AddressServiceType(..), isSharedService)
import RemoteData
import Test exposing (Test, describe, test)


apiAddress : Maybe Bool -> Api.Data.Address
apiAddress isContract =
    { actors = Nothing
    , address = "addr"
    , balance = Data.Api.values
    , currency = "btc"
    , cluster = 1
    , firstTx = { height = 1, timestamp = 0, txHash = "h" }
    , inDegree = 1
    , isContract = isContract
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
    }


suite : Test
suite =
    describe "Model.Pathfinder.Address.isSharedService"
        [ test "plain address is not a shared service" <|
            \_ ->
                isSharedService address1
                    |> Expect.equal False
        , test "exchange-tagged address is a shared service" <|
            \_ ->
                isSharedService { address1 | exchange = Just "Binance" }
                    |> Expect.equal True
        , test "smart contract is a shared service" <|
            \_ ->
                isSharedService
                    { address1 | data = RemoteData.Success (apiAddress (Just True)) }
                    |> Expect.equal True
        , test "non-contract address data alone is not a shared service" <|
            \_ ->
                isSharedService
                    { address1 | data = RemoteData.Success (apiAddress (Just False)) }
                    |> Expect.equal False
        , test "known service (e.g. swap service with actor) is a shared service" <|
            \_ ->
                isSharedService { address1 | addressServiceType = KnownService }
                    |> Expect.equal True
        , test "likely unknown service (high-traffic heuristic) is a shared service" <|
            \_ ->
                isSharedService { address1 | addressServiceType = LikelyUnknownService }
                    |> Expect.equal True
        ]
