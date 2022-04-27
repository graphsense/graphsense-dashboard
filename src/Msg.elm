module Msg exposing (Msg(..))

import Api.Data
import Browser exposing (UrlRequest)
import Http
import Locale.Msg
import Model exposing (..)
import Search.Msg
import Url exposing (Url)


type Msg
    = UserRequestsUrl UrlRequest
    | BrowserChangedUrl Url
    | BrowserGotStatistics (Result Http.Error Api.Data.Stats)
    | LocaleMsg Locale.Msg.Msg
    | SearchMsg Search.Msg.Msg
