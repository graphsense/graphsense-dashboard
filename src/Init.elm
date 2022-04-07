module Init exposing (init)

import Browser.Navigation as Nav
import Effect exposing (Effect(..), n)
import Iknaio
import Locale.Init as Locale
import Model exposing (..)
import Url exposing (Url)


init : Flags -> Url -> Nav.Key -> ( Model, Effect )
init _ url key =
    n
        { url = url
        , key = key
        , locale = Locale.init
        , theme = Iknaio.theme
        , search = ()
        , user = ()
        }
