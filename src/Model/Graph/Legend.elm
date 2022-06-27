module Model.Graph.Legend exposing (..)

import Color exposing (Color)


type alias Item =
    { color : Color
    , title : String
    , uri : String
    }
