module Test.Update.Pathfinder.Network exposing (..)

import Data.Api as Api
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Network as Data
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


suite : Test
suite =
    describe "Update.Pathfinder.Network"
        [ test "addAddress 1" <|
            \_ ->
                Network.addAddress Id.address1 Data.empty
                    |> equal Data.oneAddress
        , test "addAddress 1 again" <|
            \_ ->
                Network.addAddress Id.address1 Data.oneAddress
                    |> equal Data.oneAddress
        , test "addAddress 2" <|
            \_ ->
                Network.addAddress Id.address2 Data.oneAddress
                    |> equal Data.twoIndependentAddresses
        , test "addTx 1" <|
            \_ ->
                Network.addTx Id.tx1 Api.tx1 Data.oneAddress
                    |> equal Data.oneAddressWithTx
        , test "addTx 1 again" <|
            \_ ->
                Network.addTx Id.tx1 Api.tx1 Data.oneAddressWithTx
                    |> equal Data.oneAddressWithTx
        , test "addAddress 3" <|
            \_ ->
                Network.addAddress Id.address3 Data.oneAddressWithTx
                    |> equal Data.twoConnectedAddresses
        , test "addAddress 3 again" <|
            \_ ->
                Network.addAddress Id.address3 Data.twoConnectedAddresses
                    |> equal Data.twoConnectedAddresses
        , test "addAddress 4" <|
            \_ ->
                Network.addAddress Id.address4 Data.twoConnectedAddresses
                    |> equal Data.one2TwoAddresses
        , test "addAddress 4 again" <|
            \_ ->
                Network.addAddress Id.address4 Data.one2TwoAddresses
                    |> equal Data.one2TwoAddresses
        , test "addAddress 5" <|
            \_ ->
                Network.addAddress Id.address5 Data.one2TwoAddresses
                    |> equal Data.one2ThreeAddresses
        , test "addAddress 5 again" <|
            \_ ->
                Network.addAddress Id.address5 Data.one2ThreeAddresses
                    |> equal Data.one2ThreeAddresses
        ]
