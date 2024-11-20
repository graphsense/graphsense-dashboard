module Model.Graph.Legend exposing (Item)

import Color exposing (Color)


type alias Item =
    { color : Color
    , title : String
    , uri : String
    }
