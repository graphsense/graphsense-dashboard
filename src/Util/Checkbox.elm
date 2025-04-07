module Util.Checkbox exposing
    ( Config
    , Size(..)
    , State
    , checkbox
    , disabledState
    , removeState
    , stateFromBool
    )

import Html.Styled exposing (Attribute, Html)
import Html.Styled.Events exposing (onClick)
import RecordSetter as Rs
import Theme.Html.Icons as Icons
import Util.View exposing (pointer)


type Size
    = Large
    | Small


type State
    = Selected
    | Deselected
    | Disabled
    | Remove


type alias Config msg =
    { state : State
    , size : Size
    , msg : msg
    }


stateFromBool : Bool -> State
stateFromBool checked =
    if checked then
        Selected

    else
        Deselected


disabledState : State
disabledState =
    Disabled


removeState : State
removeState =
    Remove


checkbox : Config msg -> List (Attribute msg) -> Html msg
checkbox { state, size, msg } attrs =
    let
        attributes =
            [ pointer
            , onClick msg
            ]
                ++ attrs
    in
    case ( state, size ) of
        ( Selected, Small ) ->
            Icons.checkboxesSize14pxStateSelectedWithAttributes
                (Icons.checkboxesSize14pxStateSelectedAttributes
                    |> Rs.s_size14pxStateSelected attributes
                )
                {}

        ( Deselected, Small ) ->
            Icons.checkboxesSize14pxStateDeselectedWithAttributes
                (Icons.checkboxesSize14pxStateDeselectedAttributes
                    |> Rs.s_size14pxStateDeselected attributes
                )
                {}

        ( Selected, Large ) ->
            Icons.checkboxesSize18pxStateSelectedWithAttributes
                (Icons.checkboxesSize18pxStateSelectedAttributes
                    |> Rs.s_size18pxStateSelected attributes
                )
                {}

        ( Deselected, Large ) ->
            Icons.checkboxesSize18pxStateDeselectedWithAttributes
                (Icons.checkboxesSize18pxStateDeselectedAttributes
                    |> Rs.s_size18pxStateDeselected attributes
                )
                {}

        ( Disabled, Small ) ->
            Icons.checkboxesSize14pxStateDisabledWithAttributes
                (Icons.checkboxesSize14pxStateDisabledAttributes
                    |> Rs.s_size14pxStateDisabled attributes
                )
                {}

        ( Remove, Small ) ->
            Icons.checkboxesSize14pxStateRemoveWithAttributes
                (Icons.checkboxesSize14pxStateRemoveAttributes
                    |> Rs.s_size14pxStateRemove attributes
                )
                {}

        ( Disabled, Large ) ->
            Icons.checkboxesSize18pxStateDisabledWithAttributes
                (Icons.checkboxesSize18pxStateDisabledAttributes
                    |> Rs.s_size18pxStateDisabled attributes
                )
                {}

        ( Remove, Large ) ->
            Icons.checkboxesSize18pxStateRemoveWithAttributes
                (Icons.checkboxesSize18pxStateRemoveAttributes
                    |> Rs.s_size18pxStateRemove attributes
                )
                {}
