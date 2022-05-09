module Config.Update exposing (Config)

import Color exposing (Color)


type alias Config =
    { defaultColor : Color
    , colorScheme : List Color
    }
