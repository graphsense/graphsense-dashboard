module Locale exposing (..)

import Locale.Effect as Effect exposing (n)
import Locale.Model as Model exposing (..)
import Locale.Msg as Msg exposing (Msg)


update : Msg -> Model -> Model
update msg model =
    n model
