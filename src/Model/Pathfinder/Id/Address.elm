module Model.Pathfinder.Id.Address exposing (Id, id, network, toString)

import Model.Pathfinder.Id as Id


type Id
    = Id Id.Id


unwrap : Id -> Id.Id
unwrap (Id i) =
    i


network : Id -> String
network (Id i) =
    Id.network i


id : Id -> String
id (Id i) =
    Id.id i


toString : Id -> String
toString (Id i) =
    Id.toString i
