module Model.Pathfinder.Id exposing (..)

import Tuple exposing (first, second)


type alias Id =
    ( String, String )


network : Id -> String
network =
    first


id : Id -> String
id =
    second


toString : Id -> String
toString ( c, i ) =
    c ++ i
