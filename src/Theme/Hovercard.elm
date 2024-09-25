module Theme.Hovercard exposing (Hovercard, default)

import Color exposing (Color)


type alias Hovercard =
    { root : List ( String, String )
    , borderColor : Color
    , backgroundColor : Color
    , borderWidth : Float
    }


default : Hovercard
default =
    { root = []
    , borderColor = Color.rgba 0 0 0 0
    , backgroundColor = Color.rgba 0 0 0 0
    , borderWidth = 0
    }
