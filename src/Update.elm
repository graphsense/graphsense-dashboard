module Update exposing (update)

import Msg exposing (..)


update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 2

        Decrement ->
            model - 1
