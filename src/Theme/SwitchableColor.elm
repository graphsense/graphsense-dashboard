module Theme.SwitchableColor exposing (SwitchableColor)

import Color


type alias SwitchableColor =
    { dark : Color.Color
    , light : Color.Color
    }
