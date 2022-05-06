module Effect.Graph exposing (Effect(..), n)

import Api.Data
import Msg.Graph exposing (..)


type Effect
    = NoEffect
    | BatchEffect (List Effect)


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )
