module Test.Model.Pathfinder.TxCsvExport exposing (suite)

import Api.Data
import Data.Api as Api
import Data.Pathfinder.Address as Address
import Data.Pathfinder.Id as Id
import Dict
import Expect
import Model.Pathfinder.Id as PId
import Model.Pathfinder.Tx as Tx exposing (Io)
import Test exposing (Test)


{-| A UTXO tx with 3 inputs and 3 outputs (→ 9 full cartesian rows, no fee rows
because all test values are 0). One input and one output sit on the graph
(`Just address`), the rest are off-graph (`Nothing`).
-}
utxoTx : Tx.UtxoTx
utxoTx =
    { inputs =
        Dict.fromList
            [ ( Id.address1, Io Api.values (Just Address.address1) 1 )
            , ( Id.address2, Io Api.values Nothing 1 )
            , ( Id.address3, Io Api.values Nothing 1 )
            ]
    , outputs =
        Dict.fromList
            [ ( Id.address4, Io Api.values (Just Address.address4) 1 )
            , ( Id.address5, Io Api.values Nothing 1 )
            , ( Id.address6, Io Api.values Nothing 1 )
            ]
    , raw = Api.tx1
    }


onGraphInputId : String
onGraphInputId =
    PId.id Id.address1


onGraphOutputId : String
onGraphOutputId =
    PId.id Id.address4


run : Bool -> Maybe Int -> { rows : List Api.Data.TxAccount, capped : Bool, reduced : Bool }
run onlyVisibleIos rowCap =
    Tx.utxoTxToAccountTxs { onlyVisibleIos = onlyVisibleIos, rowCap = rowCap } Nothing utxoTx


suite : Test
suite =
    Test.describe "Model.Pathfinder.Tx.utxoTxToAccountTxs CSV export reduction"
        [ Test.test "no cap emits the full cartesian product, not capped, not reduced" <|
            \_ ->
                let
                    result =
                        run False Nothing
                in
                Expect.equal ( 9, False, False ) ( List.length result.rows, result.capped, result.reduced )
        , Test.test "cap above the full size leaves the product untouched" <|
            \_ ->
                let
                    result =
                        run False (Just 20)
                in
                Expect.equal ( 9, False, False ) ( List.length result.rows, result.capped, result.reduced )
        , Test.test "cap equal to the full size is not exceeded, so not capped" <|
            \_ ->
                let
                    result =
                        run False (Just 9)
                in
                Expect.equal ( 9, False, False ) ( List.length result.rows, result.capped, result.reduced )
        , Test.test "cap below full keeps only flows with on-graph addresses on both sides" <|
            \_ ->
                let
                    result =
                        run False (Just 6)
                in
                -- on-graph flows: on-graph input × on-graph output = 1
                Expect.equal ( 1, True, True ) ( List.length result.rows, result.capped, result.reduced )
        , Test.test "every retained row has an on-graph address on both sides" <|
            \_ ->
                let
                    result =
                        run False (Just 6)

                    hasOnGraphBothSides row =
                        row.fromAddress == onGraphInputId && row.toAddress == onGraphOutputId
                in
                result.rows
                    |> List.all hasOnGraphBothSides
                    |> Expect.equal True
        , Test.test "onlyVisible filters to both-sides on-graph flows regardless of size; reduced but not capped" <|
            \_ ->
                let
                    result =
                        run True Nothing
                in
                Expect.equal ( 1, False, True ) ( List.length result.rows, result.capped, result.reduced )
        ]
