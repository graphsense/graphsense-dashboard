module View.PageTest exposing (suite)

{-| The shared page frame: the width cap, the action in the title row, and no empty line
for a missing subtitle.

elm-css renders its stylesheet as a hidden child of the root, which is how the width cap
can be checked at all.

-}

import Expect
import Html.Styled exposing (button, text)
import Html.Styled.Attributes exposing (id)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import View.Page as Page


render : Page.Config msg -> Query.Single msg
render =
    Page.view >> Html.Styled.toUnstyled >> Query.fromHtml


page : Page.Config msg
page =
    { title = "A page title"
    , subtitle = Just "A subtitle"
    , action = Nothing
    , content = text "the content"
    }


suite : Test
suite =
    describe "View.Page"
        [ test "shows the title, the subtitle and the content" <|
            \_ ->
                render page
                    |> Expect.all
                        [ Query.has [ Selector.text "A page title" ]
                        , Query.has [ Selector.text "A subtitle" ]
                        , Query.has [ Selector.text "the content" ]
                        ]
        , test "the action sits in the same element as the title" <|
            \_ ->
                render { page | action = Just (button [ id "help" ] [ text "?" ]) }
                    -- `findAll` searches below the root: only the title row contains
                    -- both, and with the action anywhere else nothing would
                    |> Query.findAll [ Selector.containing [ Selector.id "help" ], Selector.containing [ Selector.text "A page title" ] ]
                    |> Query.count (Expect.equal 1)
        , test "without a subtitle there is no subtitle line" <|
            \_ ->
                render { page | subtitle = Nothing }
                    |> Query.children [ Selector.tag "div" ]
                    |> Query.count (Expect.equal 1)
        , test "with a subtitle there is a subtitle line" <|
            \_ ->
                render page
                    |> Query.children [ Selector.tag "div" ]
                    |> Query.count (Expect.equal 2)
        , test "is capped in width, and not centred" <|
            \_ ->
                render page
                    |> Expect.all
                        [ Query.has [ Selector.text "max-width:1440px" ]
                        , Query.hasNot [ Selector.text "margin-left:auto" ]
                        ]
        ]
