module View exposing (view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Model exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "---" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "++" ]
        ]
