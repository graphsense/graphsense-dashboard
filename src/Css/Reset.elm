module Css.Reset exposing (meyerV2, borderBoxV201408)

{-| Some CSS resets using elm-css.

@docs meyerV2, borderBoxV201408

-}

import Css exposing (..)
import Css.Global exposing (..)
import Html.Styled exposing (Html)


{-| The Eric Meyer CSS reset (2.0, public domain), converted to elm-css

Original at <https://meyerweb.com/eric/tools/css/reset/>.

-}
meyerV2 : Html msg
meyerV2 =
    global
        [ each
            [ html
            , body
            , div
            , span
            , selector "applet"
            , selector "object"
            , selector "iframe"
            , h1
            , h2
            , h3
            , h4
            , h5
            , h6
            , p
            , blockquote
            , selector "pre"
            , a
            , selector "abbr"
            , selector "acronym"
            , selector "address"
            , selector "big"
            , selector "cite"
            , code
            , selector "del"
            , selector "dfn"
            , selector "em"
            , img
            , selector "ins"
            , selector "kbd"
            , q
            , selector "s"
            , selector "samp"
            , selector "small"
            , selector "strike"
            , strong
            , selector "sub"
            , selector "sup"
            , selector "tt"
            , selector "var"
            , selector "b"
            , selector "u"
            , i
            , selector "center"
            , dl
            , dt
            , dd
            , ol
            , ul
            , li
            , fieldset
            , form
            , label
            , legend
            , selector "table"
            , caption
            , tbody
            , tfoot
            , thead
            , tr
            , th
            , td
            , article
            , aside
            , canvas
            , details
            , selector "embed"
            , selector "figure"
            , selector "figcaption"
            , footer
            , header
            , selector "hgroup"
            , menu
            , nav
            , selector "output"
            , selector "ruby"
            , section
            , summary
            , time
            , selector "mark"
            , audio
            , video
            ]
            [ margin zero
            , padding zero
            , border zero
            , fontSize (pct 100)
            , verticalAlign baseline
            , property "font" "inherit"
            ]
        , -- HTML5 display-role reset for older browsers
          each
            [ article
            , aside
            , details
            , selector "figcaption"
            , selector "figure"
            , footer
            , header
            , selector "hgroup"
            , menu
            , nav
            , section
            ]
            [ display block ]
        , body [ lineHeight (int 1) ]
        , each [ ol, ul ] [ listStyle none ]
        , each [ blockquote, q ] [ property "quotes" "none" ]
        , each [ blockquote, q ]
            [ before
                [ property "content" "''"
                , property "content" "none"
                ]
            , after
                [ property "content" "''"
                , property "content" "none"
                ]
            ]
        , selector "table"
            [ borderCollapse collapse
            , borderSpacing zero
            ]
        ]


{-| Set `box-sizing: border-box` everywhere.

See <https://www.paulirish.com/2012/box-sizing-border-box-ftw/> for more.

-}
borderBoxV201408 : Html msg
borderBoxV201408 =
    global
        [ html [ boxSizing Css.borderBox ]
        , everything
            [ boxSizing inherit
            , before [ boxSizing inherit ]
            , after [ boxSizing inherit ]
            ]
        ]
