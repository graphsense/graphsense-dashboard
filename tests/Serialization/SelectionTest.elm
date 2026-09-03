module Serialization.SelectionTest exposing (suite)

{-| "Open selection in new tab" hands the new tab a `.gs` payload of the
selection. These tests pin what goes into it and that the loader takes it
literally: a tx selected without its addresses opens as a tx with empty
address slots, not as nothing and not with neighbours pulled in.
-}

import Api.Data
import Color
import Data.Pathfinder.Id as Id
import Data.Pathfinder.Network as Network
import Dict
import Encode.Pathfinder
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Init.Pathfinder.AggEdge as AggEdge
import Json.Decode
import Model.Pathfinder exposing (Model)
import Model.Pathfinder.Deserialize exposing (Deserialized)
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..))
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter exposing (s_annotations, s_network, s_txs)
import Set
import Support.App as App
import Test exposing (Test, describe, test)
import Update.Pathfinder
import Util.Annotations as Annotations


{-| address1 -> tx1 -> address3, an agg edge between the two addresses, a note
on each address.
-}
graph : Model
graph =
    let
        network =
            Network.twoConnectedAddresses
    in
    App.model App.init
        |> s_network
            { network
                | aggEdges =
                    Dict.insert (AggEdge.initId Id.address1 Id.address3)
                        (AggEdge.init Id.address1 Id.address3 |> s_txs (Set.singleton Id.tx1))
                        network.aggEdges
            }
        |> s_annotations
            (Annotations.empty
                |> Annotations.set Id.address1 "suspicious" (Just Color.red)
                |> Annotations.set Id.address3 "plain" Nothing
            )


encodeThenRead : List MultiSelectOptions -> (Deserialized -> Expectation) -> Expectation
encodeThenRead selection f =
    Encode.Pathfinder.encodeSelection selection graph
        |> Update.Pathfinder.deserialize
        |> Result.map f
        |> Result.withDefault (Expect.fail "the selection did not decode as a .gs file")


suite : Test
suite =
    describe "encoding a selection"
        [ test "keeps only the selected nodes" <|
            \_ ->
                encodeThenRead [ MSelectedAddress Id.address1, MSelectedTx Id.tx1 ] <|
                    \d ->
                        ( List.map .id d.addresses, List.map .id d.txs )
                            |> Expect.equal ( [ Id.address1 ], [ Id.tx1 ] )
        , test "keeps the notes of the selected nodes and drops the others" <|
            \_ ->
                encodeThenRead [ MSelectedAddress Id.address1, MSelectedTx Id.tx1 ] <|
                    \d ->
                        List.map (\a -> ( a.id, a.label )) d.annotations
                            |> Expect.equal [ ( Id.address1, "suspicious" ) ]
        , test "drops an agg edge whose other end is not selected" <|
            \_ ->
                encodeThenRead [ MSelectedAddress Id.address1, MSelectedTx Id.tx1 ] <|
                    \d -> Expect.equal [] d.aggEdges
        , test "keeps an agg edge when both ends are selected" <|
            \_ ->
                encodeThenRead [ MSelectedAddress Id.address1, MSelectedAddress Id.address3 ] <|
                    \d -> List.map (\e -> ( e.a, e.b )) d.aggEdges |> Expect.equal [ ( Id.address1, Id.address3 ) ]
        , test "keeps the positions" <|
            \_ ->
                encodeThenRead [ MSelectedAddress Id.address3 ] <|
                    \d ->
                        List.map (\a -> ( a.x, a.y )) d.addresses
                            |> Expect.equal [ ( 8, 0 ) ]
        , test "selecting everything equals a plain save" <|
            \_ ->
                Encode.Pathfinder.encodeSelection
                    [ MSelectedAddress Id.address1, MSelectedAddress Id.address3, MSelectedTx Id.tx1 ]
                    graph
                    |> Expect.equal (Encode.Pathfinder.encode graph)
        , test "a tx selected without its addresses loads as a lone tx" <|
            \_ ->
                case Json.Decode.decodeString Api.Data.txDecoder Fixture.txUtxo of
                    Err e ->
                        Expect.fail (Json.Decode.errorToString e)

                    Ok tx ->
                        let
                            hash =
                                "04d92601677d62a985310b61a301e74870fa942c8be0648e16b1db23b996a8cd"

                            txId =
                                ( "btc", hash )

                            deserialized =
                                { name = "selection"
                                , addresses = []
                                , txs = [ { id = txId, x = 4, y = 0, isStartingPoint = False, index = 1 } ]
                                , annotations = []
                                , aggEdges = []
                                }

                            loaded =
                                App.init
                                    |> App.mapModel (Update.Pathfinder.fromDeserialized deserialized >> Tuple.first)
                                    |> App.step
                                        (BrowserGotBulkTxs
                                            { deserialized = deserialized, addresses = [], txs = [] }
                                            [ ( hash, tx ) ]
                                        )
                                    |> App.model
                                    |> .network
                        in
                        ( Dict.keys loaded.txs, Dict.keys loaded.addresses )
                            |> Expect.equal ( [ txId ], [] )
        ]
