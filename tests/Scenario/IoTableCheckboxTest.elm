module Scenario.IoTableCheckboxTest exposing (suite)

{-| The checkbox in a transaction's inputs/outputs list means "this address is on
the graph". It is keyed by address, not by row, so ticking one row ticks every
row that carries the same address (several inputs from one wallet, its change
output) and the header checkbox once a whole side is covered.

These scenarios pin both halves: exactly one address is added to the graph, and
the rows render according to the address they hold.

-}

import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Html.Attributes
import Json.Decode
import Model.Direction
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.TxDetails as TxDetails exposing (IoDirection(..))
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


txHash : String
txHash =
    "04d92601677d62a985310b61a301e74870fa942c8be0648e16b1db23b996a8cd"


repeatedAddress : Id
repeatedAddress =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


{-| The spec's tx example: four distinct addresses, two inputs and two outputs.
-}
withDistinctFixtures : (Api.Data.Tx -> Api.Data.Address -> Expectation) -> Expectation
withDistinctFixtures =
    withFixtures Fixture.txUtxo


{-| The same tx with both inputs and one output on the same address, as real
UTXO transactions often are.
-}
withRepeatedFixtures : (Api.Data.Tx -> Api.Data.Address -> Expectation) -> Expectation
withRepeatedFixtures =
    Fixture.txUtxo
        |> String.replace "addressB" (Tuple.second repeatedAddress)
        |> String.replace "addressD" (Tuple.second repeatedAddress)
        |> withFixtures


withFixtures : String -> (Api.Data.Tx -> Api.Data.Address -> Expectation) -> Expectation
withFixtures txJson f =
    case
        ( Json.Decode.decodeString Api.Data.txDecoder txJson
        , Json.Decode.decodeString Api.Data.addressDecoder Fixture.address
        )
    of
        ( Ok tx, Ok address ) ->
            f tx address

        ( Err e, _ ) ->
            Expect.fail ("the tx fixture did not decode: " ++ Json.Decode.errorToString e)

        ( _, Err e ) ->
            Expect.fail ("the address fixture did not decode: " ++ Json.Decode.errorToString e)


answerTx : Api.Data.Tx -> App -> App
answerTx tx =
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetTxEffect _ toMsg ->
                    Just (toMsg tx)

                _ ->
                    Nothing
        )


answerAddresses : Api.Data.Address -> App -> App
answerAddresses address =
    App.respond
        (\eff ->
            case eff of
                Effect.Api.GetAddressEffect _ toMsg ->
                    Just (toMsg address)

                _ ->
                    Nothing
        )


{-| Feeds internal effects back in and answers address requests until the flow
settles. The budget guards against an update loop.
-}
drain : Api.Data.Address -> Int -> App -> App
drain address budget app =
    if budget <= 0 || (List.isEmpty (App.internalMsgs app) && List.isEmpty (App.apiEffects app)) then
        app

    else
        app
            |> App.steps (App.internalMsgs app)
            |> answerAddresses address
            |> drain address (budget - 1)


addressesOnGraph : App -> List Id
addressesOnGraph =
    App.model >> .network >> .addresses >> Dict.keys


{-| Opens the tx deep link, answers it, and unfolds both io lists.
-}
graphWithTx : Api.Data.Tx -> App
graphWithTx tx =
    Route.txRoute { network = "btc", txHash = txHash }
        |> App.initAt
        |> answerTx tx
        |> App.step (TxDetailsMsg (TxDetails.UserClickedToggleIoTable Inputs))
        |> App.step (TxDetailsMsg (TxDetails.UserClickedToggleIoTable Outputs))


tick : Id -> Api.Data.Address -> App -> App
tick id address app =
    app
        |> App.step (TxDetailsMsg (TxDetails.UserClickedIoTableCheckbox id))
        |> drain address 10


{-| The generated checkbox renders a 14px rect when selected and a 13.5px one
when not; `rx` keeps other rects out of the count.
-}
checkboxes : String -> App -> Query.Multiple Msg
checkboxes width =
    App.html
        >> Query.findAll
            [ Selector.tag "rect"
            , Selector.attribute (Html.Attributes.attribute "rx" "2")
            , Selector.attribute (Html.Attributes.attribute "width" width)
            ]


selected : App -> Query.Multiple Msg
selected =
    checkboxes "14"


deselected : App -> Query.Multiple Msg
deselected =
    checkboxes "13.5"


suite : Test
suite =
    describe "io table checkboxes"
        [ describe "with distinct addresses"
            [ test "ticking an input adds only that address" <|
                \_ ->
                    withDistinctFixtures <|
                        \tx address ->
                            graphWithTx tx
                                |> tick ( "btc", "addressB" ) address
                                |> addressesOnGraph
                                |> Expect.equal [ ( "btc", "addressB" ) ]
            , test "ticking an output adds only that address" <|
                \_ ->
                    withDistinctFixtures <|
                        \tx address ->
                            graphWithTx tx
                                |> tick ( "btc", "addressC" ) address
                                |> addressesOnGraph
                                |> Expect.equal [ ( "btc", "addressC" ) ]
            , test "ticking one row renders exactly one selected checkbox" <|
                \_ ->
                    withDistinctFixtures <|
                        \tx address ->
                            graphWithTx tx
                                |> tick ( "btc", "addressB" ) address
                                |> selected
                                |> Query.count (Expect.equal 1)
            , test "the other rows and both headers stay deselected" <|
                \_ ->
                    withDistinctFixtures <|
                        \tx address ->
                            graphWithTx tx
                                |> tick ( "btc", "addressB" ) address
                                |> deselected
                                |> Query.count (Expect.equal 5)
            , test "the expand handle also adds only its own address" <|
                \_ ->
                    withDistinctFixtures <|
                        \tx address ->
                            graphWithTx tx
                                |> App.step
                                    (TxDetailsMsg
                                        (TxDetails.UserClickedIoTableExpand ( "btc", "addressB" ) Model.Direction.Incoming 1)
                                    )
                                |> drain address 10
                                |> addressesOnGraph
                                |> Expect.equal [ ( "btc", "addressB" ) ]
            ]
        , describe "with one address on several rows"
            [ test "ticking the unique output selects one row" <|
                \_ ->
                    withRepeatedFixtures <|
                        \tx address ->
                            graphWithTx tx
                                |> tick ( "btc", "addressC" ) address
                                |> selected
                                |> Query.count (Expect.equal 1)
            , test "ticking the repeated address selects its rows and the inputs header" <|
                \_ ->
                    -- both inputs, the inputs "all" checkbox, and the change output
                    withRepeatedFixtures <|
                        \tx address ->
                            graphWithTx tx
                                |> tick repeatedAddress address
                                |> selected
                                |> Query.count (Expect.equal 4)
            , test "but still adds a single address to the graph" <|
                \_ ->
                    withRepeatedFixtures <|
                        \tx address ->
                            graphWithTx tx
                                |> tick repeatedAddress address
                                |> addressesOnGraph
                                |> Expect.equal [ repeatedAddress ]
            ]
        ]
