module Util.Graph exposing (..)

import Config.Graph as Graph
import Json.Decode
import List.Extra
import Model.Graph.Coords exposing (BBox, Coords)
import Svg.Styled as Svg
import Svg.Styled.Events as Svg


translate : Float -> Float -> String
translate x y =
    "translate(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


rotate : Float -> String -> String
rotate degree others =
    others ++ " rotate(" ++ String.fromFloat degree ++ ")"


scale : Float -> String -> String
scale f others =
    others ++ " scale(" ++ String.fromFloat f ++ ")"


decodeCoords : (Float -> Float -> a) -> Json.Decode.Decoder a
decodeCoords decoded =
    Json.Decode.map2 decoded
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


mousedown : (Coords -> msg) -> Svg.Attribute msg
mousedown msg =
    Svg.custom "mousedown"
        (decodeCoords Coords
            |> Json.Decode.map
                (\coords ->
                    { message = msg coords
                    , stopPropagation = True
                    , preventDefault = True
                    }
                )
        )


getCategory : Graph.Config -> Maybe String -> Maybe String
getCategory gc =
    Maybe.andThen (\cat -> List.Extra.find (.id >> (==) cat) gc.entityConcepts)
        >> Maybe.map .label


getAbuse : Graph.Config -> Maybe String -> Maybe String
getAbuse gc =
    Maybe.andThen (\cat -> List.Extra.find (.id >> (==) cat) gc.abuseConcepts)
        >> Maybe.map .label
