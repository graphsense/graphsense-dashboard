module Init.Graph.ContextMenu exposing (..)

import Model.Graph.Address as Address
import Model.Graph.ContextMenu exposing (..)
import Model.Graph.Coords exposing (..)
import Model.Graph.Entity as Entity
import Model.Graph.Id as Id


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


initEntityLink : Coords -> Id.LinkId Id.EntityId -> Model
initEntityLink coords =
    EntityLink >> init coords


initAddressLink : Coords -> Id.LinkId Id.AddressId -> Model
initAddressLink coords =
    AddressLink >> init coords


initTransaction : Coords -> String -> String -> Model
initTransaction coords txHash currency =
    { coords = coords, type_ = Transaction txHash currency }
