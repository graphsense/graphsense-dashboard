module Model exposing (..)

import Api.Data
import Locale.Model as Locale
import RemoteData exposing (WebData)
import Themes.Model exposing (Theme)
import Url exposing (Url)


type alias Flags =
    {}


type alias Config =
    { getString : String -> String
    }


type alias Model navigationKey =
    { url : Url
    , key : navigationKey
    , locale : Locale.Model
    , theme : Theme
    , search : ()
    , user : ()
    , stats : WebData Api.Data.Stats
    }
