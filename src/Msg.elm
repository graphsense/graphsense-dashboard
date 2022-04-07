module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Model exposing (..)
import Url exposing (Url)


type Msg
    = UserRequestsUrl UrlRequest
    | BrowserChangedUrl Url
