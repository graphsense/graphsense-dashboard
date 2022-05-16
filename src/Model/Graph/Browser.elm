module Model.Graph.Browser exposing (Model, Type(..))

import Model.Graph.Address exposing (Address)
import Model.Graph.Entity exposing (Entity)


type alias Model =
    { type_ : Type
    , visible : Bool
    }


type Type
    = None
    | Address Address
    | Entity Entity
