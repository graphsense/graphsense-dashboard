module Init.Popup exposing (..)

import Model.Popup exposing (..)


init : String -> Float -> Float -> Model
init id x y =
    { x = x
    , y = y
    , id = id
    , size = Nothing
    }
