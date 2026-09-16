module View.SearchDropdownTest exposing (suite)

{-| The search dropdown's result list is capped so a long result list scrolls
inside the dropdown instead of growing the page past the bottom of the screen.
Read off the stylesheet elm-css injects, the only place a style is observable
without a browser.

The `!important` on the scroll rule is part of what is pinned: the generated
list sets `overflow: hidden` in a class that elm-css may order after this one,
and did for the default cap, which left the list capped but not scrollable.

-}

import Autocomplete
import Expect
import Html.Styled
import Init.Search
import Model.Search
import RecordSetter as Rs
import Support.Env as Env
import Support.MainApp as MainApp
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import View.Search


suite : Test
suite =
    describe "search dropdown height"
        [ test "is capped at half the viewport by default" <|
            \_ ->
                View.Search.searchWithMoreCss Env.viewConfig View.Search.default (openSearch (Init.Search.init (Init.Search.initSearchAddressAndTxs Nothing)))
                    |> Html.Styled.toUnstyled
                    |> Query.fromHtml
                    |> hasStyle "max-height:50vh;overflow-y:auto !important"
        , test "on the Pathfinder is nearly the whole viewport" <|
            \_ ->
                MainApp.initAt "/pathfinder"
                    |> MainApp.mapModel (\model -> Rs.s_pathfinder (Rs.s_search (openSearch model.pathfinder.search) model.pathfinder) model)
                    |> MainApp.html
                    |> hasStyle "max-height:max(10rem, calc(100vh - 12rem));overflow-y:auto !important"
        , test "on the landing page is what is left of the viewport under the box" <|
            \_ ->
                MainApp.initAt "/"
                    |> MainApp.mapModel (\model -> Rs.s_search (openSearch model.search) model)
                    |> MainApp.html
                    |> hasStyle "max-height:max(10rem, calc(100vh - 27rem));overflow-y:auto !important"
        ]


{-| A visible dropdown: focused, with a query too short to search, which
renders the minimum-input hint through the same dropdown as results do.
-}
openSearch : Model.Search.Model -> Model.Search.Model
openSearch search =
    search
        |> Rs.s_visible True
        |> Rs.s_autocomplete (Autocomplete.setQuery "a" search.autocomplete)


hasStyle : String -> Query.Single msg -> Expect.Expectation
hasStyle rule =
    Query.findAll [ Selector.tag "style", Selector.text rule ]
        >> Query.count (Expect.atLeast 1)
