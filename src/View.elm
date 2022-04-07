module View exposing (view)

import HelloWorld exposing (helloWorld)
import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)
import Msg exposing (..)


view : Int -> Html Msg
view model =
    div []
        [ img [ src "/logo.png", style "width" "400px" ] []
        , helloWorld model
        ]
