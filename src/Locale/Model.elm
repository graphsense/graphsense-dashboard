module Locale.Model exposing (..)

import Dict exposing (Dict)
import RemoteData as RD exposing (WebData)


type State
    = Empty
    | Transition (Dict String String) (Dict String String) Float
    | Settled (Dict String String)


type alias Model =
    { mapping : State
    , locale : String
    }
