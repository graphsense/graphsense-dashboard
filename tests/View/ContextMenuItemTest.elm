module View.ContextMenuItemTest exposing (suite)

{-| Plugin entries on the address context menu are links (e.g. the CSAM check),
and core greys them out on a multi-selection with `setDisabled`. That only
helps if a disabled link entry really loses its `<a href>`; core's own entries
are all message entries, so nothing else exercises this.
-}

import Html.Attributes
import Html.Styled
import Support.Env as Env
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import View.Pathfinder.ContextMenuItem as ContextMenuItem exposing (ContextMenuItem)


linkEntry : ContextMenuItem msg
linkEntry =
    ContextMenuItem.initLink2
        { icon = Html.Styled.text ""
        , text1 = "CSAM Check"
        , text2 = Nothing
        , link = "/csam-check/1Archive"
        , blank = False
        }


render : ContextMenuItem msg -> Query.Single msg
render =
    ContextMenuItem.view Env.viewConfig >> Html.Styled.toUnstyled >> Query.fromHtml


suite : Test
suite =
    describe "context menu link entry"
        [ test "is a link when enabled" <|
            \_ ->
                render linkEntry
                    |> Query.has [ Selector.tag "a", Selector.attribute (Html.Attributes.href "/csam-check/1Archive") ]
        , test "is no link once disabled" <|
            \_ ->
                render (ContextMenuItem.setDisabled True linkEntry)
                    |> Query.hasNot [ Selector.tag "a" ]
        , test "still shows its text once disabled" <|
            \_ ->
                render (ContextMenuItem.setDisabled True linkEntry)
                    |> Query.has [ Selector.text "CSAM Check" ]
        ]
