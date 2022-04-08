module Msg exposing (Msg(..))

import Api.Data
import Browser exposing (UrlRequest)
import Http
import Model exposing (..)
import Url exposing (Url)


type Msg
    = UserRequestsUrl UrlRequest
    | BrowserChangedUrl Url
    | BrowserGotStatistics (Result Http.Error Api.Data.Stats)
