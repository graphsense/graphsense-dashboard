module Test.Update.Pathfinder.Network exposing (..)

import Api.Data
import Data.Api as Api
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Network as Data
import Dict
import Expect exposing (Expectation)
import Model.Pathfinder.Network exposing (Network)
import RecordSetter exposing (..)
import Test exposing (..)
import Tuple exposing (..)
import Update.Pathfinder.Network as Network


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


suite : Test
suite =
    describe "Update.Pathfinder.Network"
        [ test "addAddress 1" <|
            \_ ->
                Network.addAddress Id.address1 Data.empty
                    |> equalIds Data.oneAddress
        , test "addAddress 1 again" <|
            \_ ->
                Network.addAddress Id.address1 Data.oneAddress
                    |> equal Data.oneAddress
        , test "addAddress 2" <|
            \_ ->
                Network.addAddress Id.address2 Data.oneAddress
                    |> equal Data.twoIndependentAddresses
        , test "add outgoing Tx 1" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx1) Data.oneAddress
                    |> Tuple.second
                    |> equalIds Data.oneAddressWithOutgoingTx
        , test "add outgoing Tx 1 again" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx1) Data.oneAddressWithOutgoingTx
                    |> Tuple.second
                    |> equal Data.oneAddressWithOutgoingTx
        , test "add incoming Tx 1" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx2) Data.oneAddress
                    |> Tuple.second
                    |> equalIds Data.oneAddressWithIncomingTx
        , test "add incoming after outgoing Tx 1" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx2) Data.oneAddressWithOutgoingTx
                    |> Tuple.second
                    |> equalIds Data.oneAddressWithTwoTxs
        , test "addAddress 3" <|
            \_ ->
                Network.addAddress Id.address3 Data.oneAddressWithOutgoingTx
                    |> equalIds Data.twoConnectedAddresses
        , test "addAddress 3 again" <|
            \_ ->
                Network.addAddress Id.address3 Data.twoConnectedAddresses
                    |> equal Data.twoConnectedAddresses
        , test "addAddress 4" <|
            \_ ->
                Network.addAddress Id.address4 Data.twoConnectedAddresses
                    |> equalIds Data.one2TwoAddresses
        , test "addAddress 4 again" <|
            \_ ->
                Network.addAddress Id.address4 Data.one2TwoAddresses
                    |> equal Data.one2TwoAddresses
        , test "addAddress 5" <|
            \_ ->
                Network.addAddress Id.address5 Data.one2TwoAddresses
                    |> equalIds Data.one2ThreeAddresses
        , test "addAddress 5 again" <|
            \_ ->
                Network.addAddress Id.address5 Data.one2ThreeAddresses
                    |> equal Data.one2ThreeAddresses
        , test "addTx 2" <|
            \_ ->
                Network.addTx (Api.Data.TxTxUtxo Api.tx3) Data.one2ThreeAddresses
                    |> Tuple.second
                    |> equalIds Data.one2TwoTxs2ThreeAddresses
        ]
