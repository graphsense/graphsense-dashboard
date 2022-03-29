module Main exposing (..)

import Browser
import Model exposing (..)
import View exposing (view)


main =
    Browser.sandbox { init = init, update = update, view = view }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1
