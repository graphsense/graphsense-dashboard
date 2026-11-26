module Scenario.GraphFilterTest exposing (suite)

{-| The on-graph filters, driven through the real update and view: what the
graph shows after a filter changes, and what a filter does not touch (undo
history, the network itself).
-}

import Dict
import Expect exposing (Expectation)
import Html.Attributes
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Selection exposing (Selection(..))
import Model.Pathfinder.Tools exposing (ToolbarHovercardType(..))
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.GraphFilter as GraphFilterMsg
import RecordSetter exposing (s_network, s_selection)
import Set
import Support.App as App exposing (App)
import Support.GraphFilterFixture exposing (accountTx, addr, chain, day, usd)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)
import Time


{-| a --(1000 $)--> b --(5 $)--> c, and d without transfers.
-}
graph : App
graph =
    App.init
        |> App.mapModel (s_network chain)
        -- a first message runs the update pipeline, as any click would
        |> App.step NoOp


filter : GraphFilterMsg.Msg -> App -> App
filter =
    GraphFilterMsg >> App.step


expectAddressNodes : Int -> App -> Expectation
expectAddressNodes n =
    App.html
        >> Query.findAll [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-address-node") ]
        >> Query.count (Expect.equal n)


byTestId : String -> Selector
byTestId =
    Html.Attributes.attribute "data-testid" >> Selector.attribute


openHovercard : App -> Maybe ToolbarHovercardType
openHovercard =
    App.model >> .toolbarHovercard >> Maybe.map Tuple.first


hiddenAddresses : App -> List Id
hiddenAddresses =
    App.model >> .highlighting >> .filter >> .hiddenAddresses >> Set.toList


suite : Test
suite =
    describe "On-graph filters"
        [ test "without a filter every address is drawn" <|
            \_ -> graph |> expectAddressNodes 4
        , test "hiding tiny transfers removes the address only they reach" <|
            \_ ->
                graph
                    |> filter GraphFilterMsg.UserToggledHideTiny
                    |> Expect.all
                        [ expectAddressNodes 3
                        , hiddenAddresses >> Expect.equal [ addr "c" ]
                        ]
        , test "a period hides the transfers outside it, and the addresses only they reach" <|
            \_ ->
                graph
                    |> filter
                        (GraphFilterMsg.UserChangedPeriod
                            (Just
                                { from = Time.millisToPosix (day 15 * 1000)
                                , until = Time.millisToPosix (day 25 * 1000)
                                }
                            )
                        )
                    |> Expect.all
                        [ expectAddressNodes 3
                        , hiddenAddresses >> Expect.equal [ addr "a" ]
                        ]
        , test "the filter leaves the network itself alone" <|
            \_ ->
                graph
                    |> filter GraphFilterMsg.UserToggledHideTiny
                    |> App.model
                    |> .network
                    |> Expect.equal chain
        , test "showing hidden edges in grey draws the address again" <|
            \_ ->
                graph
                    |> filter GraphFilterMsg.UserToggledHideTiny
                    |> filter GraphFilterMsg.UserToggledShowHidden
                    |> expectAddressNodes 4
        , test "reset brings everything back and keeps the limit" <|
            \_ ->
                graph
                    |> filter (GraphFilterMsg.UserChangedLimit 500)
                    |> filter GraphFilterMsg.UserToggledHideTiny
                    |> filter GraphFilterMsg.UserClickedReset
                    |> Expect.all
                        [ expectAddressNodes 4
                        , App.model >> .graphFilter >> .limit >> Expect.equal 500
                        ]
        , test "a selected address is greyed, not hidden" <|
            \_ ->
                graph
                    |> App.mapModel (s_selection (SelectedAddress (addr "c")))
                    |> filter GraphFilterMsg.UserToggledHideTiny
                    |> Expect.all
                        [ expectAddressNodes 4
                        , App.model >> .highlighting >> .filter >> .greyedAddresses >> Set.toList >> Expect.equal [ addr "c" ]
                        ]
        , test "a transfer added while the filter is on is filtered on arrival" <|
            \_ ->
                graph
                    |> filter GraphFilterMsg.UserToggledHideTiny
                    |> App.mapModel
                        (\m ->
                            let
                                network =
                                    m.network

                                tx =
                                    accountTx "t3" "a" "d" "eth" (usd 1) (day 30)
                            in
                            { m | network = { network | txs = Dict.insert tx.id tx network.txs } }
                        )
                    |> App.step NoOp
                    |> hiddenAddresses
                    |> Expect.equal [ addr "c", addr "d" ]
        , test "filters are not part of undo" <|
            \_ ->
                graph
                    |> filter GraphFilterMsg.UserToggledHideTiny
                    |> App.step UserClickedUndo
                    |> App.model
                    |> .graphFilter
                    |> .hideTiny
                    |> Expect.equal True
        , test "assets toggle case-insensitively" <|
            \_ ->
                graph
                    |> filter (GraphFilterMsg.UserToggledAsset "USDT")
                    |> filter (GraphFilterMsg.UserToggledAsset "usdt")
                    |> App.model
                    |> .graphFilter
                    |> .excludedAssets
                    |> Expect.equal Set.empty
        , describe "the filter UI"
            [ test "the toolbar button opens and closes the filter popover" <|
                \_ ->
                    graph
                        |> App.html
                        |> Query.find [ byTestId "gs-toolbar-filter" ]
                        |> Event.simulate Event.click
                        |> Event.expect (GraphFilterMsg GraphFilterMsg.UserClickedToggleFilter)
            , test "toggling opens the popover, toggling again closes it" <|
                \_ ->
                    graph
                        |> filter GraphFilterMsg.UserClickedToggleFilter
                        |> Expect.all
                            [ openHovercard >> Expect.equal (Just Filter)
                            , filter GraphFilterMsg.UserClickedToggleFilter >> openHovercard >> Expect.equal Nothing
                            ]
            , test "the close button closes it and the limit editor" <|
                \_ ->
                    graph
                        |> filter GraphFilterMsg.UserClickedToggleFilter
                        |> filter GraphFilterMsg.UserClickedChangeLimit
                        |> filter GraphFilterMsg.UserClickedCloseFilter
                        |> Expect.all
                            [ openHovercard >> Expect.equal Nothing
                            , App.model >> .graphFilter >> .editingLimit >> Expect.equal False
                            ]
            , test "no badge and no status row while no filter is on" <|
                \_ ->
                    graph
                        |> App.html
                        |> Expect.all
                            [ Query.findAll [ byTestId "gs-filter-count" ] >> Query.count (Expect.equal 0)
                            , Query.findAll [ byTestId "gs-filter-status" ] >> Query.count (Expect.equal 0)
                            ]
            , test "the badge counts the active filters" <|
                \_ ->
                    graph
                        |> filter GraphFilterMsg.UserToggledHideTiny
                        |> filter (GraphFilterMsg.UserToggledAsset "eth")
                        |> App.html
                        |> Query.find [ byTestId "gs-filter-count" ]
                        |> Query.has [ Selector.text "2" ]
            , test "the status row says how many edges are hidden, one chip per filter" <|
                \_ ->
                    graph
                        |> filter GraphFilterMsg.UserToggledHideTiny
                        |> App.html
                        |> Query.find [ byTestId "gs-filter-status" ]
                        |> Expect.all
                            [ Query.find [ byTestId "gs-filter-summary" ] >> Query.has [ Selector.text "1 edge hidden" ]
                            , Query.findAll [ byTestId "gs-filter-chip" ] >> Query.count (Expect.equal 1)
                            ]
            , test "a chip's x removes that filter" <|
                \_ ->
                    graph
                        |> filter (GraphFilterMsg.UserToggledAsset "eth")
                        |> App.html
                        |> Query.find [ byTestId "gs-filter-chip-remove" ]
                        |> Event.simulate Event.click
                        |> Event.expect (GraphFilterMsg GraphFilterMsg.UserClickedClearAssets)
            , test "clearing the assets brings every asset back" <|
                \_ ->
                    graph
                        |> filter (GraphFilterMsg.UserToggledAsset "eth")
                        |> filter (GraphFilterMsg.UserToggledAsset "usdt")
                        |> filter GraphFilterMsg.UserClickedClearAssets
                        |> expectAddressNodes 4
            ]
        ]
