module Util.View exposing (..)

import Html.Styled exposing (Attribute, Html, span)
import Html.Styled.Attributes exposing (classList)


none : Html msg
none =
    span [] []


nona : Attribute msg
nona =
    classList []


aa : (a -> Attribute msg) -> Maybe a -> List (Attribute msg) -> List (Attribute msg)
aa toAttr value =
    (++) (value |> Maybe.map (toAttr >> List.singleton) |> Maybe.withDefault [])
