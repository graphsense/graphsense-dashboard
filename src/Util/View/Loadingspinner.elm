module Util.View.Loadingspinner exposing (html, svg)

import Css
import Html.Styled exposing (Attribute, Html)
import Html.Styled.Attributes exposing (css)
import String.Format
import Svg.Styled exposing (Svg, animate, g, rect)
import Svg.Styled.Attributes exposing (attributeName, begin, dur, fill, height, keyTimes, repeatCount, rx, ry, transform, values, viewBox, width, x, y)
import Theme.Colors as Colors


{-| The SVG loading spinner element.
-}
spinner : List (Svg msg)
spinner =
    let
        rectFill : String
        rectFill =
            Colors.brandPrimary

        animatedRect : String -> Svg msg
        animatedRect beginTime =
            rect
                [ x "43.5"
                , y "25.5"
                , rx "6.5"
                , ry "6.5"
                , width "13"
                , height "13"
                , fill rectFill
                ]
                [ animate
                    [ attributeName "opacity"
                    , values "1;0"
                    , keyTimes "0;1"
                    , dur "1s"
                    , begin beginTime
                    , repeatCount "indefinite"
                    ]
                    []
                ]
    in
    [ g [ transform "rotate(0 50 50)" ] [ animatedRect "-0.875s" ]
    , g [ transform "rotate(45 50 50)" ] [ animatedRect "-0.75s" ]
    , g [ transform "rotate(90 50 50)" ] [ animatedRect "-0.625s" ]
    , g [ transform "rotate(135 50 50)" ] [ animatedRect "-0.5s" ]
    , g [ transform "rotate(180 50 50)" ] [ animatedRect "-0.375s" ]
    , g [ transform "rotate(225 50 50)" ] [ animatedRect "-0.25s" ]
    , g [ transform "rotate(270 50 50)" ] [ animatedRect "-0.125s" ]
    , g [ transform "rotate(315 50 50)" ] [ animatedRect "0s" ]
    ]


svgWidth : Float
svgWidth =
    100


svgHeight : Float
svgHeight =
    100


svg : Float -> Float -> Svg msg
svg width height =
    g
        [ "scale({{ }}, {{ }})"
            |> String.Format.value (String.fromFloat (width / svgWidth))
            |> String.Format.value (String.fromFloat (height / svgHeight))
            |> transform
        ]
        spinner


{-| Render an inline SVG loading spinner with the brand primary color.
-}
html : List (Attribute msg) -> Html msg
html attributes =
    Html.Styled.div (css [ Css.width <| Css.px 24, Css.height <| Css.px 24 ] :: attributes)
        [ Svg.Styled.svg
            [ "0 0 {{ }} {{ }}"
                |> String.Format.value (String.fromFloat svgWidth)
                |> String.Format.value (String.fromFloat svgHeight)
                |> viewBox
            , width "100%"
            , height "100%"
            , Svg.Styled.Attributes.style "shape-rendering: auto; display: block;"
            ]
            spinner
        ]
