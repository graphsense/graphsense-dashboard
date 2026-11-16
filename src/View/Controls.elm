module View.Controls exposing (Size(..), ToggleConfig, checkboxWithLabel, checkboxWithLabelAndTitle, radio, radioSmall, segmentedToggle, toggle, toggleWithText)

import Css
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (css, title)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Theme.Html.SelectionControls as Sc
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
checkboxWithLabel config =
    checkboxWithLabelWithAttributes [] config


{-| Like `checkboxWithLabel`, but attaches an HTML `title` tooltip (a longer
description of what the option does) to the whole control.
-}
checkboxWithLabelAndTitle : { label : String, title : String, checked : Bool, disabled : Bool, msg : msg, size : Size } -> Html msg
checkboxWithLabelAndTitle config =
    checkboxWithLabelWithAttributes
        [ title config.title ]
        { label = config.label
        , checked = config.checked
        , disabled = config.disabled
        , msg = config.msg
        , size = config.size
        }


checkboxWithLabelWithAttributes : List (Html.Styled.Attribute msg) -> { label : String, checked : Bool, disabled : Bool, msg : msg, size : Size } -> Html msg
checkboxWithLabelWithAttributes rootAttributes { label, checked, disabled, size, msg } =
    Sc.checkboxWithLabelWithAttributes
        { root = rootAttributes
        , label = []
        }
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
toggleCell =
    toggleCellWith []


toggleCellWith : List (Html.Styled.Attribute msg) -> { a | title : String, selected : Bool, msg : msg } -> Html msg
toggleCellWith attrs { selected, title, msg } =
    Sc.toggleCellWithTextWithAttributes
        (Sc.toggleCellWithTextAttributes
            |> Rs.s_root
                (Util.View.pointer
                    :: Util.View.onClickWithStop msg
                    :: attrs
                )
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


{-| A row of text cells, one selected: the two-cell `toggleWithText` with any
number of cells. Uses the generated switch's container styles; the container
itself has only the two fixed cell slots.
-}
segmentedToggle : List { title : String, selected : Bool, msg : msg, attributes : List (Html.Styled.Attribute msg) } -> Html msg
segmentedToggle cells =
    Html.Styled.div
        [ css Sc.toggleSwitchText_details.styles ]
        (List.map (\cell -> toggleCellWith cell.attributes cell) cells)
