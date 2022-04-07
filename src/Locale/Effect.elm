module Locale.Effect exposing (Effect(..), n)


type Effect
    = NoEffect


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )
