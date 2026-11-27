module Scenario.SelectionStatsPanelTest exposing (suite)

import Dict
import Expect
import Html.Attributes
import Init.Pathfinder.Address as Address
import Init.Pathfinder.Id as Id
import Model.Pathfinder.Selection exposing (MultiSelectOptions(..), Selection(..))
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter exposing (s_selection)
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector)


byTestId : String -> Selector
byTestId =
    Html.Attributes.attribute "data-testid" >> Selector.attribute


twoSelected : App
twoSelected =
    App.init
        |> App.mapModel
            (s_selection
                (MultiSelect
                    [ MSelectedAddress (Id.init "eth" "0xa")
                    , MSelectedAddress (Id.init "eth" "0xb")
                    ]
                )
            )


suite : Test
suite =
    describe "The multi-selection panel"
        [ test "shows for a multi-selection" <|
            \_ -> twoSelected |> App.html |> Query.findAll [ byTestId "gs-selection-stats" ] |> Query.count (Expect.equal 1)
        , test "its close button closes the panel like the other panels" <|
            \_ ->
                twoSelected
                    |> App.html
                    |> Query.find [ byTestId "gs-selection-stats-close" ]
                    |> Event.simulate Event.click
                    |> Event.expect UserClosedDetailsView
        , test "closed, it is gone and nothing is selected" <|
            \_ ->
                twoSelected
                    |> App.step UserClosedDetailsView
                    |> Expect.all
                        [ App.html >> Query.findAll [ byTestId "gs-selection-stats" ] >> Query.count (Expect.equal 0)
                        , App.model >> .selection >> Expect.equal NoSelection
                        ]
        , test "clearing it from the toolbar unmarks the nodes, so the selection is not stuck" <|
            \_ ->
                -- a real multi-selection marks each node selected; closing
                -- used to reset only `model.selection`, leaving the nodes
                -- drawn as selected and out of reach of the next selection
                twoSelectedOnGraph
                    |> App.mapModel
                        (\m ->
                            let
                                network =
                                    m.network
                            in
                            { m | network = { network | addresses = Dict.map (\_ a -> { a | selected = True }) network.addresses } }
                        )
                    |> App.step UserClosedDetailsView
                    |> App.model
                    |> (\m -> ( m.selection, m.network.addresses |> Dict.values |> List.filter .selected |> List.length ))
                    |> Expect.equal ( NoSelection, 0 )
        , test "its clear button closes the selection like the panel's close" <|
            \_ ->
                twoSelectedOnGraph
                    |> App.html
                    |> Query.find [ byTestId "gs-selection-toolbar-clear" ]
                    |> Event.simulate Event.click
                    |> Event.expect UserClosedDetailsView
        , test "a floating toolbar shows the actions for the selection" <|
            \_ ->
                twoSelectedOnGraph
                    |> App.html
                    |> Query.find [ byTestId "gs-selection-toolbar" ]
                    |> Query.findAll [ byTestId "gs-selection-toolbar-delete" ]
                    |> Query.count (Expect.equal 1)
        , test "its delete removes the selection, like the toolbar's delete" <|
            \_ ->
                twoSelectedOnGraph
                    |> App.html
                    |> Query.find [ byTestId "gs-selection-toolbar-delete" ]
                    |> Event.simulate Event.click
                    |> Event.expect UserClickedToolbarDeleteIcon
        , test "no toolbar without a multi-selection" <|
            \_ -> App.init |> App.html |> Query.findAll [ byTestId "gs-selection-toolbar" ] |> Query.count (Expect.equal 0)
        ]


{-| The toolbar is placed over the selected nodes, so they must be on the graph.
-}
twoSelectedOnGraph : App
twoSelectedOnGraph =
    twoSelected
        |> App.mapModel
            (\m ->
                let
                    network =
                        m.network
                in
                { m
                    | network =
                        { network
                            | addresses =
                                [ "0xa", "0xb" ]
                                    |> List.map (\a -> ( Id.init "eth" a, Address.init (Id.init "eth" a) { x = 0, y = 0 } ))
                                    |> Dict.fromList
                        }
                }
            )
