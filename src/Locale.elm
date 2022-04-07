module Locale.Update exposing (update)

import Locale.Effect exposing (Effect, n)
import Locale.Model exposing (Model)
import Locale.Msg exposing (Msg(..))


update : Msg -> Model -> ( Model, Effect )
update msg model =
    n model
