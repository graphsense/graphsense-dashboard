module Scenario.SubTxHoverTest exposing (suite)

{-| An account transaction often triggers internal or token transfers of its
own, each a separate edge on the graph (`<hash>`, `<hash>_I660`, ...).
Hovering any one of them highlights the whole family, but they stay
independently selectable.
-}

import Api.Data
import Config.Pathfinder exposing (HideForExport(..), TracingMode(..))
import Dict
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Update.Pathfinder.Network as Network


baseHash : String
baseHash =
    "b61413c495fdad6114a7aa863a00b2e3c28945979a10885b12b30316ea9f072c"


main_ : Id
main_ =
    ( "eth", baseHash )


internal1 : Id
internal1 =
    ( "eth", baseHash ++ "_I660" )


internal2 : Id
internal2 =
    ( "eth", baseHash ++ "_I662" )


unrelated : Id
unrelated =
    ( "eth", "04d92601677d62a985310b61a301e74870fa942c8be0648e16b1db23b996a8cd" )


accountTx : String -> String -> Api.Data.TxAccount -> Api.Data.Tx
accountTx txHash identifier raw =
    Api.Data.TxTxAccount { raw | txHash = txHash, identifier = identifier }


{-| The graph holds the tx, two of its internal transfers and an unrelated tx.
-}
withGraph : (App -> Expectation) -> Expectation
withGraph f =
    case Json.Decode.decodeString Api.Data.txAccountDecoder Fixture.txAccount of
        Ok raw ->
            let
                pc =
                    { snapToGrid = False
                    , highlightClusterFriends = False
                    , tracingMode = TransactionTracingMode
                    , avoidOverlapingNodes = True
                    , hideForExport = NoExport
                    }

                add txHash identifier network =
                    Network.addTx pc (accountTx txHash identifier raw) network
                        |> Tuple.second
            in
            App.init
                |> App.mapModel
                    (\m ->
                        { m
                            | network =
                                m.network
                                    |> add baseHash baseHash
                                    |> add baseHash (baseHash ++ "_I660")
                                    |> add baseHash (baseHash ++ "_I662")
                                    |> add (Tuple.second unrelated) (Tuple.second unrelated)
                        }
                    )
                |> f

        Err error ->
            Expect.fail ("fixture did not decode: " ++ Json.Decode.errorToString error)


hoveredTxs : App -> List Id
hoveredTxs app =
    (App.model app).network.txs
        |> Dict.filter (\_ tx -> tx.hovered)
        |> Dict.keys


selectedTxs : App -> List Id
selectedTxs app =
    (App.model app).network.txs
        |> Dict.filter (\_ tx -> tx.selected)
        |> Dict.keys


suite : Test
suite =
    describe "Hovering an account tx or one of its sub txs"
        [ test "hovering the tx highlights its sub txs" <|
            \_ ->
                withGraph
                    (App.step (UserMovesMouseOverTx main_)
                        >> hoveredTxs
                        >> Expect.equal (List.sort [ main_, internal1, internal2 ])
                    )
        , test "hovering a sub tx highlights the tx and the other sub txs" <|
            \_ ->
                withGraph
                    (App.step (UserMovesMouseOverTx internal2)
                        >> hoveredTxs
                        >> Expect.equal (List.sort [ main_, internal1, internal2 ])
                    )
        , test "hovering an unrelated tx highlights only that tx" <|
            \_ ->
                withGraph
                    (App.step (UserMovesMouseOverTx unrelated)
                        >> hoveredTxs
                        >> Expect.equal [ unrelated ]
                    )
        , test "moving out unhovers the whole family" <|
            \_ ->
                withGraph
                    (App.steps [ UserMovesMouseOverTx internal1, UserMovesMouseOutTx internal1 ]
                        >> hoveredTxs
                        >> Expect.equal []
                    )
        , test "moving to another tx unhovers the previous family" <|
            \_ ->
                withGraph
                    (App.steps [ UserMovesMouseOverTx internal1, UserMovesMouseOverTx unrelated ]
                        >> hoveredTxs
                        >> Expect.equal [ unrelated ]
                    )
        , test "a sub tx is selected on its own" <|
            \_ ->
                withGraph
                    (App.mapModel (\m -> { m | modPressed = True })
                        >> App.step (UserClickedTx internal1)
                        >> selectedTxs
                        >> Expect.equal [ internal1 ]
                    )
        ]
