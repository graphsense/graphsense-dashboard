module Init.Graph.Adding exposing (addresses, entities, init)

import Dict
import Model.Graph.Adding exposing (..)
import Model.Graph.Id as Id
import Set


init : Model
init =
    { addresses = Dict.empty
    , entities = Dict.empty
    , labels = Set.empty
    , addressPath = []
    , entityPath = []
    }


addresses : Maybe ( Bool, Id.AddressId ) -> AddingAddress
addresses anchor =
    { address = Nothing
    , entity = Nothing
    , outgoing = Nothing
    , incoming = Nothing
    , anchor = anchor
    }


entities : AddingEntity
entities =
    { entity = Nothing
    , outgoing = Nothing
    , incoming = Nothing
    }
