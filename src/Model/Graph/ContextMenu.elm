module Model.Graph.ContextMenu exposing (..)

import Model.Graph.Address exposing (Address)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Id exposing (AddressId, EntityId, LinkId)


type Type
    = Address Address
    | Entity Entity


type alias Model =
    { coords : Coords
    , type_ : Type
    }
