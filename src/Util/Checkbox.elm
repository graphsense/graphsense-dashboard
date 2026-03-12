module Util.Checkbox exposing
    ( Config
    , bigSize
    , checkbox
    , disabledState
    , removeState
    , smallSize
    , stateFromBool
    )

import Html.Styled exposing (Attribute, Html)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Theme.Html.SelectionControls exposing (CheckboxesSize(..), CheckboxesState(..), checkboxesAttributes, checkboxesWithAttributes)
import Util.View exposing (pointer)


type alias Config msg =
    { state : CheckboxesState
    , size : CheckboxesSize
    , msg : Maybe msg
    }


stateFromBool : Bool -> CheckboxesState
stateFromBool checked =
    if checked then
        CheckboxesStateSelected

    else
        CheckboxesStateDeselected


disabledState : CheckboxesState
disabledState =
    CheckboxesStateDisabled


removeState : CheckboxesState
removeState =
    CheckboxesStateRemove


smallSize : CheckboxesSize
smallSize =
    CheckboxesSize14px


bigSize : CheckboxesSize
bigSize =
    CheckboxesSize18px


checkbox : Config msg -> List (Attribute msg) -> Html msg
checkbox { state, size, msg } attrs =
    let
        attributes =
            (msg |> Maybe.map (onClick >> List.singleton >> (::) pointer) |> Maybe.withDefault [])
                ++ attrs
    in
    checkboxesWithAttributes
        (checkboxesAttributes
            |> Rs.s_root attributes
        )
        { root =
            { size = size
            , state = state
            }
        }
