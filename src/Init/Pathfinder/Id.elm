module Init.Pathfinder.Id exposing (init)

import Model.Pathfinder.Id exposing (Id)
import Tuple exposing (pair)


init : String -> String -> Id
init =
    pair
