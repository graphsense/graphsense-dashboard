module View.Controls exposing (Size(..), ToggleConfig, checkboxWithLabel, lightModeToggle, radio, radioSmall, tabs, tabsSmallItems, toggle, toggleCell, toggleWithIcons, toggleWithText)

import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Theme.Html.SelectionControls as Sc
import Theme.Html.SettingsPage as Sp
import Util.Checkbox as Checkbox
import Util.View


type alias ToggleConfig msg =
    { size : Sc.SwitchSize
    , selected : Bool
    , disabled : Bool
    , msg : msg
    }


type Size
    = Small
    | Large


checkboxWithLabel : { label : String, checked : Bool, disabled : Bool, msg : msg, size : Size } -> Html msg
checkboxWithLabel { label, checked, disabled, size, msg } =
    Sc.checkboxWithLabel
        { root = { label = label }
        , checkboxes =
            { variant =
                Checkbox.checkbox
                    { state =
                        if checked && not disabled then
                            Sc.CheckboxesStateSelected

                        else if disabled then
                            Sc.CheckboxesStateDisabled

                        else
                            Sc.CheckboxesStateDeselected
                    , size =
                        case size of
                            Large ->
                                Sc.CheckboxesSize18px

                            Small ->
                                Sc.CheckboxesSize14px
                    , msg =
                        if disabled then
                            Nothing

                        else
                            Just msg
                    }
                    []
            }
        }


radioSmall : String -> Bool -> msg -> Html msg
radioSmall name selected msg =
    radio
        { label = name
        , selected = selected
        , disabled = False
        , msg = msg
        , size = Small
        }


radio : { label : String, selected : Bool, disabled : Bool, size : Size, msg : msg } -> Html msg
radio { label, selected, disabled, size, msg } =
    Sc.radioItemWithAttributes
        (Sc.radioItemAttributes
            |> Rs.s_root
                (if disabled then
                    []

                 else
                    [ onClick msg, [ Css.cursor Css.pointer ] |> css ]
                )
        )
        { radio =
            { variant =
                Sc.radio
                    { root =
                        { state =
                            if selected && not disabled then
                                Sc.RadioStateOn

                            else if disabled then
                                Sc.RadioStateDisabled

                            else
                                Sc.RadioStateOff
                        , size =
                            case size of
                                Small ->
                                    Sc.RadioSizeSmall

                                Large ->
                                    Sc.RadioSizeLarge
                        }
                    }
            }
        , root = { radioLabel = label }
        }


toggle : ToggleConfig msg -> Html msg
toggle { size, selected, disabled, msg } =
    Sc.switchWithAttributes
        (Sc.switchAttributes
            |> Rs.s_root
                (if disabled then
                    []

                 else
                    [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
                )
        )
        { root =
            { state =
                if disabled then
                    Sc.SwitchStateDisabled

                else if selected then
                    Sc.SwitchStateOn

                else
                    Sc.SwitchStateOff
            , size = size
            }
        }


toggleWithText : { selectedA : Bool, titleA : String, titleB : String, msg : msg } -> Html msg
toggleWithText { selectedA, titleA, titleB, msg } =
    Sc.toggleSwitchTextWithInstances
        (Sc.toggleSwitchTextAttributes
            |> Rs.s_root [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
        )
        Sc.toggleSwitchTextInstances
        { rightCell =
            { variant =
                toggleCell
                    { title = titleB
                    , selected = not selectedA
                    , msg = msg
                    }
            }
        , leftCell =
            { variant =
                toggleCell
                    { title = titleA
                    , selected = selectedA
                    , msg = msg
                    }
            }
        }


toggleCell : { title : String, selected : Bool, msg : msg } -> Html msg
toggleCell { selected, title, msg } =
    Sc.toggleCellWithTextWithAttributes
        (Sc.toggleCellWithTextAttributes
            |> Rs.s_root
                [ Util.View.pointer
                , Util.View.onClickWithStop msg
                ]
        )
        { root =
            { state =
                if selected then
                    Sc.ToggleCellWithTextStateSelected

                else
                    Sc.ToggleCellWithTextStateDeselected
            , toggleText = title
            }
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
                                { root = { toggleText = "" } }
                        )
                , iconB
                    |> Maybe.map
                        (\i ->
                            Sc.toggleCellWithTextStateDeselectedWithInstances
                                Sc.toggleCellWithTextStateDeselectedAttributes
                                (Sc.toggleCellWithTextStateDeselectedInstances
                                    |> Rs.s_placeholder (Just i)
                                )
                                { root = { toggleText = "" } }
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
                                { root = { toggleText = "" } }
                        )
                , iconB
                    |> Maybe.map
                        (\i ->
                            Sc.toggleCellWithTextStateSelectedWithInstances
                                Sc.toggleCellWithTextStateSelectedAttributes
                                (Sc.toggleCellWithTextStateSelectedInstances
                                    |> Rs.s_placeholder (Just i)
                                )
                                { root = { toggleText = "" } }
                        )
                )

        shapeAttrs =
            [ css [ Css.cursor Css.pointer ], Util.View.onClickWithStop msg ]
    in
    if selectedA then
        Sc.modeToggleModeLightWithInstances
            (Sc.modeToggleModeLightAttributes
                |> Rs.s_root shapeAttrs
            )
            (Sc.modeToggleModeLightInstances
                |> Rs.s_iconsDarkMode b
                |> Rs.s_iconsLightMode a
            )
            {}

    else
        Sc.modeToggleModeDarkWithInstances
            (Sc.modeToggleModeDarkAttributes
                |> Rs.s_root shapeAttrs
            )
            (Sc.modeToggleModeDarkInstances
                |> Rs.s_iconsLightMode a
                |> Rs.s_iconsDarkMode b
            )
            {}


viewTab : Sc.SingleTabSize -> { a | msg : msg, selected : Bool, title : String } -> Html msg
viewTab size t =
    Sc.singleTabWithAttributes
        (Sc.singleTabAttributes
            |> Rs.s_root [ Util.View.onClickWithStop t.msg, css [ Css.cursor Css.pointer ] ]
        )
        { root =
            { state =
                if t.selected then
                    Sc.SingleTabStateSelected

                else
                    Sc.SingleTabStateNeutral
            , size = size
            , tabLabel = t.title
            }
        }


tabs : Sc.SingleTabSize -> List { title : String, selected : Bool, msg : msg } -> Html msg
tabs size tbs =
    div
        [ Html.Styled.Attributes.css
            Sp.settingsPageSettingsTabsSettingsTabs_details.styles
        ]
        (tbs |> List.map (viewTab size))


tabsSmallItems : List { title : String, selected : Bool, msg : msg } -> List (Html msg)
tabsSmallItems =
    List.map (viewTab Sc.SingleTabSizeSmall)
