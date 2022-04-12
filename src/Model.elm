module Model exposing (..)

import Addon exposing (Addon)
import Api.Data
import Locale.Model as Locale
import RemoteData exposing (WebData)
import Themes.Model exposing (Theme)
import Url exposing (Url)


type alias Flags =
    {}


type alias Config =
    { theme : Theme
    , addons : List Addon
    }


type alias Env =
    { locale : Locale.Model
    }


type alias Model navigationKey =
    { url : Url
    , key : navigationKey
    , config : Config
    , locale : Locale.Model
    , search : ()
    , user : ()
    , stats : WebData Api.Data.Stats
    }
