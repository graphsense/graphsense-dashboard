module Locale.Msg exposing (Msg(..))

import Dict exposing (Dict)
import Http


type Msg
    = BrowserLoadedTranslation String (Result Http.Error (Dict String String))
