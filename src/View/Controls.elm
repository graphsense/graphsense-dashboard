module View.Controls exposing (lightModeToggle, tabs, toggle, toggleSmall, toggleWithIcons, toggleWithText)

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


toggleSmall : { selected : Bool, disabled : Bool, msg : msg } -> Html msg
toggleSmall { selected, disabled, msg } =
    case ( selected, disabled ) of
        ( True, True ) ->
            Sc.switchStateDisabledSizeSmall {}

        ( True, False ) ->
            Sc.switchStateOnSizeSmallWithAttributes
                (Sc.switchStateOnSizeSmallAttributes
                    |> Rs.s_stateOnSizeSmall [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
                )
                {}

        ( False, False ) ->
            Sc.switchStateOffSizeSmallWithAttributes
                (Sc.switchStateOffSizeSmallAttributes
                    |> Rs.s_stateOffSizeSmall [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
                )
                {}

        ( False, True ) ->
            Sc.switchStateDisabledSizeSmall {}


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


lightModeToggle : { selectedA : Bool, msg : msg } -> Html msg
lightModeToggle { selectedA, msg } =
    toggleWithIconsInternal { selectedA = selectedA, iconA = Nothing, iconB = Nothing, msg = msg }


toggleWithIcons : { selectedA : Bool, iconA : Html msg, iconB : Html msg, msg : msg } -> Html msg
toggleWithIcons { selectedA, iconA, iconB, msg } =
    toggleWithIconsInternal { selectedA = selectedA, iconA = Just iconA, iconB = Just iconB, msg = msg }


toggleWithIconsInternal : { selectedA : Bool, iconA : Maybe (Html msg), iconB : Maybe (Html msg), msg : msg } -> Html msg
toggleWithIconsInternal { selectedA, iconA, iconB, msg } =
    let
        ( a, b ) =
            if selectedA then
                ( iconA
                    |> Maybe.map
                        (\i ->
                            Sc.toggleCellWithTextStateSelectedWithInstances
                                Sc.toggleCellWithTextStateSelectedAttributes
                                (Sc.toggleCellWithTextStateSelectedInstances
                                    |> Rs.s_placeholder (Just i)
                                )
                                { stateSelected = { toggleText = "" } }
                        )
                , iconB
                    |> Maybe.map
                        (\i ->
                            Sc.toggleCellWithTextStateDeselectedWithInstances
                                Sc.toggleCellWithTextStateDeselectedAttributes
                                (Sc.toggleCellWithTextStateDeselectedInstances
                                    |> Rs.s_placeholder (Just i)
                                )
                                { stateDeselected = { toggleText = "" } }
                        )
                )

            else
                ( iconA
                    |> Maybe.map
                        (\i ->
                            Sc.toggleCellWithTextStateDeselectedWithInstances
                                Sc.toggleCellWithTextStateDeselectedAttributes
                                (Sc.toggleCellWithTextStateDeselectedInstances
                                    |> Rs.s_placeholder (Just i)
                                )
                                { stateDeselected = { toggleText = "" } }
                        )
                , iconB
                    |> Maybe.map
                        (\i ->
                            Sc.toggleCellWithTextStateSelectedWithInstances
                                Sc.toggleCellWithTextStateSelectedAttributes
                                (Sc.toggleCellWithTextStateSelectedInstances
                                    |> Rs.s_placeholder (Just i)
                                )
                                { stateSelected = { toggleText = "" } }
                        )
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
                |> Rs.s_iconsDarkMode b
                |> Rs.s_iconsLightMode a
            )
            {}

    else
        Sc.modeToggleModeDarkWithInstances
            (Sc.modeToggleModeDarkAttributes
                |> Rs.s_modeDark shapeAttrs
            )
            (Sc.modeToggleModeDarkInstances
                |> Rs.s_iconsLightMode a
                |> Rs.s_iconsDarkMode b
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