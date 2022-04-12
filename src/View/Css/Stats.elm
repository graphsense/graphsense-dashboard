module View.Css.Stats exposing (..)

import Css exposing (..)
import View.Env exposing (Env)


root : Env -> List Style
root env =
    [ env.scaled 1
        |> px
        |> padding
    ]


currency : Env -> List Style
currency env =
    [ env.scaled 1
        |> px
        |> padding
    ]


currencyHeading : Env -> List Style
currencyHeading env =
    [ env.scaled 1
        |> px
        |> padding
    ]
