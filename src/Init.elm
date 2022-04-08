module Init exposing (init)

import Api.Data
import Browser.Navigation as Nav
import Effect exposing (Effect(..), n)
import Iknaio
import Locale.Init as Locale
import Model exposing (..)
import Url exposing (Url)


init : Flags -> Url -> key -> ( Model key, Effect )
init _ url key =
    ( { url = url
      , key = key
      , locale = Locale.init
      , theme = Iknaio.theme
      , search = ()
      , user = ()
      , stats = Api.Data.Stats Nothing Nothing Nothing
      }
    , GetStatisticsEffect
    )
