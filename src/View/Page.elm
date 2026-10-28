module View.Page exposing (Config, view)

{-| The frame of a plugin's own page with a title.

Every plugin used to style the generated `Theme.Html.Page.pageWithTitle` itself, and
they had drifted apart: one capped at 1400px, one at 1800px, others not at all, and two
rebuilt the same title row to fit a help button into it. Switching between them in the
sidebar moved the title and the content around. This is the one place that decides it.

Core's statistics page keeps its own full-width layout.

-}

import Css
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import RecordSetter as Rs
import Theme.Html.Page as Page


{-| `action` sits at the right end of the title row, where the Pathfinder keeps its help
button. A page without a subtitle gets no empty line in its place.
-}
type alias Config msg =
    { title : String
    , subtitle : Maybe String
    , action : Maybe (Html msg)
    , content : Html msg
    }


{-| Left-aligned and capped rather than centred: the title then stays next to the sidebar
on every page, and on a wide screen a table's columns do not drift so far apart that a
row is hard to follow across. 1440px still fits a full bech32 address in a table row.
-}
maxWidth : Float
maxWidth =
    1440


{-| The frame scrolls itself when its content is taller than the window: the generated
root hides its overflow, which cut off a long page instead of scrolling it.
`minHeight 0` is what lets it shrink to the window in the first place, and it also lets a page that wants only one part of itself to scroll
(a table, a list) give that part `minHeight 0` and `overflow: auto` in turn.
-}
view : Config msg -> Html msg
view config =
    Page.pageWithTitleWithInstances
        (Page.pageWithTitleAttributes
            |> Rs.s_root
                [ css
                    [ Css.flexGrow (Css.num 1)
                    , Css.minHeight (Css.px 0)
                    , Css.width (Css.pct 100)
                    , Css.maxWidth (Css.px maxWidth)
                    , Css.overflowY Css.auto |> Css.important
                    ]
                ]
        )
        (Page.pageWithTitleInstances
            |> Rs.s_title (config.action |> Maybe.map (titleRow config.title))
            |> Rs.s_subtitle
                (case config.subtitle of
                    -- not `Util.View.none`: an empty span is still a flex item and
                    -- still gets the root's gap; an empty text node is no item at all
                    Nothing ->
                        Just (text "")

                    Just _ ->
                        Nothing
                )
        )
        { root =
            { title = config.title
            , subtitle = Maybe.withDefault "" config.subtitle
            , content = config.content
            }
        }


{-| The generated root aligns its children at the start, so the row needs the full width
for the action to reach the right edge.
-}
titleRow : String -> Html msg -> Html msg
titleRow title action =
    div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            , Css.width (Css.pct 100)
            ]
        ]
        [ div [ css Page.pageWithTitleTitle_details.styles ] [ text title ]
        , action
        ]
