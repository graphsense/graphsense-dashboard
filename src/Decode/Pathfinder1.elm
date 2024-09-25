module Decode.Pathfinder1 exposing (decoder)

import Init.Pathfinder.Id as Id
import Json.Decode exposing (..)
import Model.Pathfinder.Deserialize exposing (..)
import Model.Pathfinder.Id exposing (Id)


decoder : Decoder Deserialized
decoder =
    map2 Deserialized
        (index 2 (list thingDecoder))
        (index 3 (list thingDecoder))


thingDecoder : Decoder DeserializedThing
thingDecoder =
    map4 DeserializedThing
        (index 0 idDecoder)
        (index 1 float)
        (index 2 float)
        (index 3 bool)


idDecoder : Decoder Id
idDecoder =
    map2 Id.init
        (index 0 string)
        (index 1 string)
