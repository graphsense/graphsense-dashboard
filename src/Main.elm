module Main exposing (main)

import Browser
import Msg exposing (Msg)
import Update exposing (update)
import View exposing (view)


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }
