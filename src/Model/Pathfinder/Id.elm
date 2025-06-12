module Model.Pathfinder.Id exposing (Id, id, network, toString)

import Tuple exposing (first, second)


type alias Id =
    ( String, String )


type alias AggEdgeId =
    String


network : Id -> String
network =
    first


id : Id -> String
id =
    second


toString : Id -> String
toString ( c, i ) =
    c ++ i
