module Decode.Pathfinder1 exposing (decoder)

import Color exposing (Color)
import Init.Pathfinder.Id as Id
import Json.Decode exposing (Decoder, bool, float, index, list, map, map2, map3, map4, map5, maybe, oneOf, string, succeed)
import Model.Pathfinder.Deserialize exposing (Deserialized, DeserializedAggEdge, DeserializedAnnotation, DeserializedThing)
import Model.Pathfinder.Id exposing (Id)
import Set


decoder : Decoder Deserialized
decoder =
    map5 Deserialized
        (index 2 string)
        (index 3 (list thingDecoder))
        (index 4 (list thingDecoder))
        (index 5 (list annotationDecoder))
        (oneOf [ index 6 (list aggEdgeDecoder), succeed [] ])


aggEdgeDecoder : Decoder DeserializedAggEdge
aggEdgeDecoder =
    map3 DeserializedAggEdge
        (index 0 idDecoder)
        (index 1 idDecoder)
        (index 2 (list idDecoder |> map Set.fromList))


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
