module Hovercard exposing (Config, hovercard)

{-| This module makes rendering hovercards like [Wikipedia's](https://anandchowdhary.github.io/hovercard/) easy.

@docs Config, hovercard

-}

import Browser.Dom as Dom
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HA
import Svg exposing (Svg)
import Svg.Attributes as SA


{-| Configure the hovercard.

  - maxWidth: maximum width of the hovercard
  - maxHeight: maximum height of the hovercard
  - tickLength: length of the tick
  - borderColor, borderWidth, backgroundColor: minimal styling for the hovercard and the small arrow pointing to the element

-}
type alias Config =
    { maxWidth : Int
    , maxHeight : Int
    , tickLength : Float
    , borderColor : Color
    , backgroundColor : Color
    , borderWidth : Float
    }


{-| Render a hovercard above or below the given [Browser.Dom.Element](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#Element).

Call this function at the root of your HTML so the hovercard is positioned correctly.

Example:

    hovercard
        -- configuration
        { maxWidth = 100
        , maxHeight = 100
        , borderColor = Color.black
        , backgroundColor = Color.lightBlue
        , borderWidth = 2
        }
        -- Browser.Dom.Element representing
        -- viewport and position of the element
        element
        -- additional styles for the hovercard, eg. a shadow
        [ style "box-shadow" "5px 5px 5px 0px rgba(0,0,0,0.25)"
        ]
        -- the content of the hovercard
        [ div
            []
            [ text "Lorem ipsum dolor sit amet"
            ]
        ]

-}
hovercard : Config -> Dom.Element -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
hovercard { maxWidth, maxHeight, tickLength, borderColor, backgroundColor, borderWidth } element attr hoverContent =
    let
        el =
            element.element

        vp =
            element.viewport

        -- position of el relative to vp
        x =
            el.x
                - vp.x
                |> max 0

        y =
            el.y
                - vp.y
                |> max 0

        w =
            min el.width vp.width

        h =
            min el.height vp.height

        diffBelow =
            vp.height
                - y
                - h

        diffAbove =
            y

        diffRight =
            vp.width - x

        baselineBottom =
            diffAbove
                < toFloat maxHeight
                && not (diffBelow < toFloat maxHeight)

        anchorH =
            if diffRight < toFloat maxWidth then
                "right"

            else
                "left"

        ( anchorV, arrange ) =
            if diffAbove < toFloat maxHeight then
                ( "top", List.reverse )

            else
                ( "bottom", identity )

        mw =
            toFloat maxWidth
                |> min vp.width

        mh =
            toFloat maxHeight
                |> min vp.height
    in
    Html.div
        [ HA.style "position" "absolute"
        , HA.style "top" <|
            (String.fromFloat <|
                if baselineBottom then
                    el.y + el.height

                else
                    el.y
            )
                ++ "px"
        , HA.style "left" <| String.fromFloat el.x ++ "px"
        , HA.style "width" <| String.fromFloat el.width ++ "px"
        ]
        [ Html.div
            [ HA.style "position" "absolute"
            , HA.style "max-width" <| String.fromFloat mw ++ "px"
            , HA.style "max-height" <| String.fromFloat mh ++ "px"
            , HA.style anchorH "0"
            , HA.style "z-index" "100"
            , HA.style anchorV "100%"
            ]
            ([ Html.div
                ([ HA.style "overflow" "auto"
                 , HA.style "position" "relative"
                 , HA.style anchorV <| String.fromFloat (tickLength / 2) ++ "px"
                 , HA.style "z-index" "1"
                 , Color.toCssString backgroundColor
                    |> HA.style "background-color"
                 , Color.toCssString borderColor
                    |> HA.style "border-color"
                 , String.fromFloat borderWidth
                    ++ "px"
                    |> HA.style "border-width"
                 , HA.style "border-style" "solid"
                 ]
                    ++ attr
                )
                hoverContent
             , triangle
                { length = tickLength
                , borderColor = borderColor
                , backgroundColor = backgroundColor
                , borderWidth = borderWidth
                , flip = anchorV == "bottom"
                }
                [ HA.style "position" "absolute"
                , HA.style anchorH "1"
                , HA.style anchorV "0"
                , HA.style "z-index" "2"
                ]
             ]
                |> arrange
            )
        ]


triangle : { length : Float, borderColor : Color, backgroundColor : Color, borderWidth : Float, flip : Bool } -> List (Svg.Attribute msg) -> Svg msg
triangle { length, borderColor, backgroundColor, borderWidth, flip } attr =
    let
        tl =
            String.fromFloat length

        tl2 =
            String.fromFloat <| length / 2

        tl3 =
            String.fromFloat <| length / 2 + borderWidth

        path =
            if flip then
                "M 0,0 " ++ tl2 ++ "," ++ tl2 ++ " " ++ tl ++ ",0"

            else
                "M 0," ++ tl3 ++ " " ++ tl2 ++ ",1" ++ " " ++ tl ++ "," ++ tl3
    in
    Svg.svg
        ([ SA.width tl
         , SA.height tl3
         ]
            ++ attr
        )
        [ Svg.path
            [ SA.d path
            , SA.strokeLinecap "round"
            , borderWidth * 1.5 |> String.fromFloat |> SA.strokeWidth
            , Color.toCssString borderColor
                |> SA.stroke
            ]
            []
        , Svg.path
            [ SA.d path
            , Color.toCssString backgroundColor |> SA.fill
            ]
            []
        ]
