module View.AutocompleteTest exposing (suite)

{-| The dropdown's `result` styles go on the list of rows, which is a flex
column. Capping that column's height makes its items shrink to fit, because
every generated row has `overflow: hidden`, so a hundred suggestions once
squashed into the cap a few pixels tall each. The rows have to be pinned
to their natural height whenever the list can scroll.

The assertions read the stylesheet elm-css injects next to the markup: it
is the only place a style is observable without a browser.

-}

import Css
import Html.Styled exposing (div, text, toUnstyled)
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import View.Autocomplete as Autocomplete


suite : Test
suite =
    describe "View.Autocomplete.dropdownStyled"
        [ test "applies the result styles to the row list" <|
            \_ ->
                stylesheet
                    |> Query.has [ Selector.text "max-height:300px" ]
        , test "pins the rows to their natural height so a capped list scrolls instead of squashing them" <|
            \_ ->
                stylesheet
                    |> Query.has [ Selector.text "flex-shrink:0" ]
        ]


stylesheet : Query.Single ()
stylesheet =
    Autocomplete.dropdownStyled
        { frame = []
        , result = [ Css.maxHeight (Css.px 300), Css.overflowY Css.auto ]
        , loadingSpinner = text ""
        }
        Env.viewConfig
        { loading = False, visible = True, onClick = () }
        [ div [] [ text "a row" ] ]
        |> toUnstyled
        |> Query.fromHtml
        |> Query.find [ Selector.tag "style" ]
