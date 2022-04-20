module Model exposing (..)

import Api.Data
import Html exposing (Attribute, Html)
import Locale.Model as Locale
import RemoteData exposing (WebData)
import Theme exposing (Theme)
import Url exposing (Url)


type alias Flags =
    { locale : String
    }


type alias Config =
    { theme : Theme
    }


type alias Model navigationKey =
    { url : Url
    , key : navigationKey
    , locale : Locale.Model
    , search : ()
    , user : ()
    , stats : WebData Api.Data.Stats
    }
