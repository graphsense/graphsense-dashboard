module Decode.Pathfinder1 exposing (decoder)

import Color exposing (Color)
import Init.Pathfinder.Id as Id
import Json.Decode exposing (Decoder, bool, float, index, list, map2, map3, map4, maybe, string)
import Model.Pathfinder.Deserialize exposing (Deserialized, DeserializedAnnotation, DeserializedThing)
import Model.Pathfinder.Id exposing (Id)


decoder : Decoder Deserialized
decoder =
    map4 Deserialized
        (index 2 string)
        (index 3 (list thingDecoder))
        (index 4 (list thingDecoder))
        (index 5 (list annotationDecoder))


annotationDecoder : Decoder DeserializedAnnotation
annotationDecoder =
    map3 DeserializedAnnotation
        (index 0 idDecoder)
        (index 1 string)
        (maybe (index 2 decodeColor))


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


decodeColor : Decoder Color
decodeColor =
    map4 (\r g b a -> Color.fromRgba { red = r, green = g, blue = b, alpha = a })
        (index 0 float)
        (index 1 float)
        (index 2 float)
        (index 3 float)
