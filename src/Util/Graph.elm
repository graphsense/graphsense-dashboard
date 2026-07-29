module Util.Graph exposing (decodeCoords, mousedown, translate)

import Json.Decode
import Model.Graph.Coords exposing (Coords)
import Svg.Styled as Svg
import Svg.Styled.Events as Svg


translate : Float -> Float -> String
translate x y =
    "translate(" ++ String.fromFloat x ++ ", " ++ String.fromFloat y ++ ")"


decodeCoords : (Float -> Float -> a) -> Json.Decode.Decoder a
decodeCoords decoded =
    Json.Decode.map2 decoded
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


mousedown : (Coords -> msg) -> Svg.Attribute msg
mousedown msg =
    Svg.custom "mousedown"
        (Json.Decode.field "button" Json.Decode.int
            |> Json.Decode.andThen
                (\button ->
                    if button == 0 then
                        decodeCoords Coords
                            |> Json.Decode.map
                                (\coords ->
                                    { message = msg coords
                                    , stopPropagation = True
                                    , preventDefault = True
                                    }
                                )

                    else
                        Json.Decode.fail "ignore non-left mouse button"
                )
        )
