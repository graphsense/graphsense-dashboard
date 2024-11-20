module Init.Pathfinder.Id exposing (init, initClusterId)

import Hex
import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (pair)


init : String -> String -> Id
init network =
    pair (String.toLower network)


initClusterId : String -> Int -> Id
initClusterId network =
    Hex.toString
        >> init network
