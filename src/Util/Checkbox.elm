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
import Theme.Html.Icons as Icons
import Util.View exposing (pointer)


type alias Config msg =
    { state : Icons.CheckboxesState
    , size : Icons.CheckboxesSize
    , msg : msg
    }


stateFromBool : Bool -> Icons.CheckboxesState
stateFromBool checked =
    if checked then
        Icons.CheckboxesStateSelected

    else
        Icons.CheckboxesStateDeselected


disabledState : Icons.CheckboxesState
disabledState =
    Icons.CheckboxesStateDisabled


removeState : Icons.CheckboxesState
removeState =
    Icons.CheckboxesStateRemove


smallSize : Icons.CheckboxesSize
smallSize =
    Icons.CheckboxesSize14px


bigSize : Icons.CheckboxesSize
bigSize =
    Icons.CheckboxesSize18px


checkbox : Config msg -> List (Attribute msg) -> Html msg
checkbox { state, size, msg } attrs =
    let
        attributes =
            [ pointer
            , onClick msg
            ]
                ++ attrs
    in
    Icons.checkboxesWithAttributes
        (Icons.checkboxesAttributes
            |> Rs.s_root attributes
        )
        { root =
            { size = size
            , state = state
            }
        }
