module View.Popup exposing (..)

import Draggable
import Html exposing (Attribute, Html, div, img, span, text)
import Html.Attributes exposing (id, style)
import Model exposing (Msg(..))
import Model.Popup exposing (..)


view : Model -> List (Html Msg) -> Html Msg
view model =
    div
        [ id model.id
        , style "position" "absolute"
        , style "top" <| String.fromFloat model.y ++ "px"
        , style "left" <| String.fromFloat model.x ++ "px"
        , style "visible" <|
            {- if model.size == Nothing then
                   "hidden"

               else
            -}
            "visible"
        , Draggable.mouseTrigger model.id DragMsg
        ]
