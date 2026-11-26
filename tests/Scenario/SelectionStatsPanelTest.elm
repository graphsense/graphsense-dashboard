module Scenario.SelectionStatsPanelTest exposing (suite)

import Expect
import Html.Attributes
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
        ]
