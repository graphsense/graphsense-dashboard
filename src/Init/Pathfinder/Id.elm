module Init.Pathfinder.Id exposing (init, initClusterId, initClusterIdFromRecord, initFromRecord)

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


initFromRecord : { t | address : String, currency : String } -> Id
initFromRecord { address, currency } =
    init currency address


initClusterIdFromRecord : { t | entity : Int, currency : String } -> Id
initClusterIdFromRecord { entity, currency } =
    initClusterId currency entity
