module Graph.Effect exposing (Effect(..), n)

import Api.Data
import Graph.Msg exposing (..)


type Effect
    = NoEffect
    | BatchEffect (List Effect)


n : model -> ( model, Effect )
n model =
    ( model, NoEffect )
