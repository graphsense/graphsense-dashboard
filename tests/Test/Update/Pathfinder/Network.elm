module Test.Update.Pathfinder.Network exposing (suite)

import Animation
import Api.Data
import Data.Api as Api
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Network as Data
import Dict
import Expect exposing (Expectation)
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Network exposing (Network)
import Test exposing (Test)
import Tuple
import Update.Pathfinder.Network as Network exposing (FindPosition(..))


equal : Network -> Network -> Expectation
equal expected result =
    Expect.all
        [ \e -> Expect.equalDicts e.addresses result.addresses
        , \e -> Expect.equalDicts e.txs result.txs
        ]
        expected


equalIds : Network -> Network -> Expectation
equalIds expected result =
    Expect.all
        [ \e -> Expect.equalLists (e.addresses |> Dict.keys) (result.addresses |> Dict.keys)
        , \e -> Expect.equalLists (e.txs |> Dict.keys) (result.txs |> Dict.keys)
        ]
        expected


equalCoords : Network -> Network -> Expectation
equalCoords expected result =
    let
        toCoords a =
            { id = a.id
            , x = a.x
            , y = Animation.getTo a.y
            }

        resultTxs =
            result.txs |> Dict.values |> List.map toCoords

        resultAddresses =
            result.addresses |> Dict.values |> List.map toCoords
    in
    Expect.all
        [ \e -> Expect.equalLists (e.addresses |> Dict.values |> List.map toCoords) resultAddresses
        , \e -> Expect.equalLists (e.txs |> Dict.values |> List.map toCoords) resultTxs
        ]
        expected


suite : Test
suite =
    Test.describe "Update.Pathfinder.Network"
        [ Test.test "addAddress 1" <|
            \_ ->
                Network.addAddress Id.address1 Data.empty
                    |> equal Data.oneAddress
        , Test.test "addAddress 1 again" <|
            \_ ->
                Network.addAddress Id.address1 Data.oneAddress
                    |> equal Data.oneAddress
        , Test.test "addAddress 2" <|
            \_ ->
                Network.addAddress Id.address2 Data.oneAddress
                    |> equal Data.twoIndependentAddresses
        , Test.test "add outgoing Tx 1" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx1) Data.oneAddress
                    |> Tuple.second
                    |> equalIds Data.oneAddressWithOutgoingTx
        , Test.test "add outgoing Tx 1 again" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx1) Data.oneAddressWithOutgoingTx
                    |> Tuple.second
                    |> equal Data.oneAddressWithOutgoingTx
        , Test.test "add incoming Tx 1" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx2) Data.oneAddress
                    |> Tuple.second
                    |> equalCoords Data.oneAddressWithIncomingTx
        , Test.test "add incoming after outgoing Tx 1" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx2) Data.oneAddressWithOutgoingTx
                    |> Tuple.second
                    |> equalCoords Data.oneAddressWithTwoTxs
        , Test.test "addAddress 3" <|
            \_ ->
                Network.addAddress Id.address3 Data.oneAddressWithOutgoingTx
                    |> equalCoords Data.twoConnectedAddresses
        , Test.test "addAddress 3 again" <|
            \_ ->
                Network.addAddress Id.address3 Data.twoConnectedAddresses
                    |> equal Data.twoConnectedAddresses
        , Test.test "addAddress 4" <|
            \_ ->
                Network.addAddress Id.address4 Data.twoConnectedAddresses
                    |> equalCoords Data.one2TwoAddresses
        , Test.test "addAddress 4 again" <|
            \_ ->
                Network.addAddress Id.address4 Data.one2TwoAddresses
                    |> equal Data.one2TwoAddresses
        , Test.test "addAddress 5" <|
            \_ ->
                Network.addAddress Id.address5 Data.one2TwoAddresses
                    |> equalCoords Data.one2ThreeAddresses
        , Test.test "addAddress 5 again" <|
            \_ ->
                Network.addAddress Id.address5 Data.one2ThreeAddresses
                    |> equal Data.one2ThreeAddresses
        , Test.test "addTx 2" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx3) Data.one2ThreeAddresses
                    |> Tuple.second
                    |> equalCoords Data.one2TwoTxs2ThreeAddresses
        , Test.test "add overlapping tx+address" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx4) Data.one2TwoTxs2ThreeAddresses
                    |> Tuple.second
                    |> Network.addAddressWithPosition (NextTo ( Outgoing, Id.tx4 )) Id.address8
                    |> equalCoords Data.one2TwoTxs2ThreeAddressesWithOverlapping
        ]
