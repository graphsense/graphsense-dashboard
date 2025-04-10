module Util.Graph exposing (decodeCoords, filterTxValue, mousedown, rotate, scale, translate)

import Api.Data
import Config.Graph as Graph
import Dict exposing (Dict)
import Json.Decode
import Model.Graph.Coords exposing (Coords)
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


filterTxValue : Graph.Config -> String -> Api.Data.Values -> Maybe (Dict String Api.Data.Values) -> Bool
filterTxValue gc _ value tokenValues =
    gc.showZeroTransactions
        || List.any (.value >> (/=) 0)
            (tokenValues
                |> Maybe.map Dict.values
                |> Maybe.withDefault []
            )
        || value.value
        /= 0
