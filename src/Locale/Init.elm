module Locale.Init exposing (init)

import Dict
import Locale.Effect exposing (Effect(..))
import Locale.Model as Model exposing (Model, State(..))


init : String -> ( Model, Effect )
init locale =
    ( { mapping = Empty
      , locale = locale
      }
    , GetTranslationEffect locale
    )
