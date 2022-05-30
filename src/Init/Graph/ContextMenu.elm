module Init.Graph.ContextMenu exposing (..)

import Model.Graph.Address as Address
import Model.Graph.ContextMenu exposing (..)
import Model.Graph.Coords exposing (..)
import Model.Graph.Entity as Entity
import Model.Graph.Id exposing (..)


initAddress : Coords -> Address.Address -> Model
initAddress coords =
    Address
        >> init coords


initEntity : Coords -> Entity.Entity -> Model
initEntity coords =
    Entity
        >> init coords


init : Coords -> Type -> Model
init =
    Model
