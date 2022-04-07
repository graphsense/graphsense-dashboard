module Model exposing (..)

import Browser.Navigation as Nav
import Locale.Model as Locale
import Themes.Model exposing (Theme)
import Url exposing (Url)


type alias Flags =
    {}


type alias Config =
    { getString : String -> String
    }


type alias Model =
    { url : Url
    , key : Nav.Key
    , locale : Locale.Model
    , theme : Theme
    , search : ()
    , user : ()
    }
