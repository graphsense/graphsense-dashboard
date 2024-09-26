module View.Controls exposing (tabs, toggleWithIcons, toggleWithText)

import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import RecordSetter as Rs
import Theme.Html.SelectionControls as Sc
import Util.View


toggleWithText : { selectedA : Bool, titleA : String, titleB : String, msg : msg } -> Html msg
toggleWithText { selectedA, titleA, titleB, msg } =
    let
        ( a, b ) =
            if selectedA then
                ( Sc.toggleCellSelected { toggleCellSelected = { toggleText = titleA } }
                , Sc.toggleCellNeutral { toggleCellNeutral = { toggleText = titleB } }
                )

            else
                ( Sc.toggleCellNeutral { toggleCellNeutral = { toggleText = titleA } }
                , Sc.toggleCellSelected { toggleCellSelected = { toggleText = titleB } }
                )
    in
    Sc.toggleSwitchTextWithInstances
        (Sc.toggleSwitchTextAttributes
            |> Rs.s_toggleSwitchText [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
        )
        (Sc.toggleSwitchTextInstances
            |> Rs.s_toggleCellSelected (Just a)
            |> Rs.s_toggleCellNeutral (Just b)
        )
        { toggleCellNeutral = { toggleText = "" }
        , toggleCellSelected = { toggleText = "" }
        }


toggleWithIcons : { selectedA : Bool, iconA : Html msg, iconB : Html msg, msg : msg } -> Html msg
toggleWithIcons { selectedA, iconA, iconB, msg } =
    let
        ( a, b ) =
            if selectedA then
                ( Sc.toggleCellSelectedWithInstances
                    Sc.toggleCellSelectedAttributes
                    (Sc.toggleCellSelectedInstances
                        |> Rs.s_placeholder (Just iconA)
                    )
                    { toggleCellSelected = { toggleText = "" } }
                , Sc.toggleCellNeutralWithInstances
                    Sc.toggleCellNeutralAttributes
                    (Sc.toggleCellNeutralInstances
                        |> Rs.s_placeholder (Just iconB)
                    )
                    { toggleCellNeutral = { toggleText = "" } }
                )

            else
                ( Sc.toggleCellNeutralWithInstances
                    Sc.toggleCellNeutralAttributes
                    (Sc.toggleCellNeutralInstances
                        |> Rs.s_placeholder (Just iconA)
                    )
                    { toggleCellNeutral = { toggleText = "" } }
                , Sc.toggleCellSelectedWithInstances
                    Sc.toggleCellSelectedAttributes
                    (Sc.toggleCellSelectedInstances
                        |> Rs.s_placeholder (Just iconB)
                    )
                    { toggleCellSelected = { toggleText = "" } }
                )
    in
    Sc.toggleSwitchIconsWithInstances
        (Sc.toggleSwitchIconsAttributes
            |> Rs.s_toggleSwitchIcons [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
        )
        (Sc.toggleSwitchIconsInstances
            |> Rs.s_toggleCellSelected (Just a)
            |> Rs.s_toggleCellNeutral (Just b)
        )
        {}


tabs : List { title : String, selected : Bool, msg : msg } -> Html msg
tabs tbs =
    let
        viewTab t =
            if t.selected then
                Sc.singleTabStateSelectedWithAttributes
                    Sc.singleTabStateSelectedAttributes
                    { stateSelected = { tabLabel = t.title } }

            else
                Sc.singleTabStateNeutralWithAttributes
                    (Sc.singleTabStateNeutralAttributes
                        |> Rs.s_stateNeutral [ Util.View.onClickWithStop t.msg, css [ Css.cursor Css.pointer ] ]
                    )
                    { stateNeutral = { tabLabel = t.title } }
    in
    div
        [ Html.Styled.Attributes.css
            [ Css.borderRadius (Css.px 0)
            , Css.opacity (Css.num 1)
            , Css.height (Css.px 30)
            , Css.width (Css.pct 100)
            , Css.position Css.relative
            , Css.displayFlex
            , Css.boxSizing Css.borderBox
            , Css.property "gap" "0px"
            , Css.alignItems Css.center
            , Css.justifyContent Css.center
            , Css.displayFlex
            , Css.flexDirection Css.row
            , Css.paddingBottom (Css.px 0)
            , Css.paddingTop (Css.px 0)
            , Css.paddingRight (Css.px 0)
            , Css.paddingLeft (Css.px 0)
            , Css.backgroundColor (Css.rgba 0 0 0 0)
            , Css.borderWidth (Css.px 1)
            ]
        ]
        (tbs |> List.map viewTab)
