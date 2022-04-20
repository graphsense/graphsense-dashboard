module Locale.Init exposing (init)

import Dict
import Locale.Effect exposing (Effect(..))
import Locale.Model as Model exposing (Model)


init : String -> ( Model, Effect )
init locale =
    ( { mapping = Dict.empty
      , locale = locale
      }
    , GetTranslationEffect locale
    )
