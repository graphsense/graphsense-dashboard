module Locale.Msg exposing (Msg(..))

import Dict exposing (Dict)
import Http
import Time


type Msg
    = BrowserLoadedTranslation String (Result Http.Error (Dict String String))
    | RuntimeTick Float
    | BrowserSentTimezone Time.Zone
