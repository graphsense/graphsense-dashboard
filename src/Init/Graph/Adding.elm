module Init.Graph.Adding exposing (..)

import Dict
import Model.Graph.Adding exposing (..)
import Set


init : Model
init =
    { addresses = Dict.empty
    , entities = Dict.empty
    , labels = Set.empty
    }


addresses : AddingAddress
addresses =
    { address = Nothing
    , entity = Nothing
    , outgoing = Nothing
    , incoming = Nothing
    }


entities : AddingEntity
entities =
    { entity = Nothing
    , outgoing = Nothing
    , incoming = Nothing
    }
