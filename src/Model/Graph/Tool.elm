module Model.Graph.Tool exposing (..)

import Html exposing (Html)


type alias Tool msg =
    { icon : Html msg
    , title : String
    , msg : msg
    }
