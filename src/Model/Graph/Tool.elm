module Model.Graph.Tool exposing (..)

import Color exposing (Color)
import Html exposing (Html)


type alias Tool msg =
    { icon : Html msg
    , title : String
    , msg : String -> msg
    , color : Maybe Color
    }
