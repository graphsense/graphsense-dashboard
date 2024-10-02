module View.Controls exposing (tabs, toggle, toggleWithIcons, toggleWithText)

import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import RecordSetter as Rs
import Theme.Html.SelectionControls as Sc
import Theme.Html.SettingsPage as Sp
import Util.View


toggle : { selected : Bool, disabled : Bool, msg : msg } -> Html msg
toggle { selected, disabled, msg } =
    case ( selected, disabled ) of
        ( True, True ) ->
            Sc.switchStateDisabledSizeBig {}

        ( True, False ) ->
            Sc.switchStateOnSizeBigWithAttributes
                (Sc.switchStateOnSizeBigAttributes
                    |> Rs.s_stateOnSizeBig [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
                )
                {}

        ( False, False ) ->
            Sc.switchStateOffSizeBigWithAttributes
                (Sc.switchStateOffSizeBigAttributes
                    |> Rs.s_stateOffSizeBig [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
                )
                {}

        ( False, True ) ->
            Sc.switchStateDisabledSizeBig {}


toggleWithText : { selectedA : Bool, titleA : String, titleB : String, msg : msg } -> Html msg
toggleWithText { selectedA, titleA, titleB, msg } =
    let
        ( a, b ) =
            if selectedA then
                ( Sc.toggleCellWithTextStateSelected { stateSelected = { toggleText = titleA } }
                , Sc.toggleCellWithTextStateDeselected { stateDeselected = { toggleText = titleB } }
                )

            else
                ( Sc.toggleCellWithTextStateDeselected { stateDeselected = { toggleText = titleA } }
                , Sc.toggleCellWithTextStateSelected { stateSelected = { toggleText = titleB } }
                )
    in
    Sc.toggleSwitchTextWithInstances
        (Sc.toggleSwitchTextAttributes
            |> Rs.s_toggleSwitchText [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
        )
        Sc.toggleSwitchTextInstances
        { rightCell = { variant = b }
        , leftCell = { variant = a }
        }


toggleWithIcons : { selectedA : Bool, iconA : Html msg, iconB : Html msg, msg : msg } -> Html msg
toggleWithIcons { selectedA, iconA, iconB, msg } =
    let
        ( a, b ) =
            if selectedA then
                ( Sc.toggleCellWithTextStateSelectedWithInstances
                    Sc.toggleCellWithTextStateSelectedAttributes
                    (Sc.toggleCellWithTextStateSelectedInstances
                        |> Rs.s_placeholder (Just iconA)
                    )
                    { stateSelected = { toggleText = "" } }
                , Sc.toggleCellWithTextStateDeselectedWithInstances
                    Sc.toggleCellWithTextStateDeselectedAttributes
                    (Sc.toggleCellWithTextStateDeselectedInstances
                        |> Rs.s_placeholder (Just iconB)
                    )
                    { stateDeselected = { toggleText = "" } }
                )

            else
                ( Sc.toggleCellWithTextStateDeselectedWithInstances
                    Sc.toggleCellWithTextStateDeselectedAttributes
                    (Sc.toggleCellWithTextStateDeselectedInstances
                        |> Rs.s_placeholder (Just iconA)
                    )
                    { stateDeselected = { toggleText = "" } }
                , Sc.toggleCellWithTextStateSelectedWithInstances
                    Sc.toggleCellWithTextStateSelectedAttributes
                    (Sc.toggleCellWithTextStateSelectedInstances
                        |> Rs.s_placeholder (Just iconB)
                    )
                    { stateSelected = { toggleText = "" } }
                )

        shapeAttrs =
            [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
    in
    if selectedA then
        Sc.modeToggleModeLightWithInstances
            (Sc.modeToggleModeLightAttributes
                |> Rs.s_modeLight shapeAttrs
            )
            (Sc.modeToggleModeLightInstances
                |> Rs.s_toggleCellSelected (Just a)
                |> Rs.s_toggleCellNeutral (Just b)
            )
            {}

    else
        Sc.modeToggleModeDarkWithInstances
            (Sc.modeToggleModeDarkAttributes
                |> Rs.s_modeDark shapeAttrs
            )
            (Sc.modeToggleModeDarkInstances
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
            Sp.settingsPageSettingsTabsSettingsTabs_details.styles
        ]
        (tbs |> List.map viewTab)
