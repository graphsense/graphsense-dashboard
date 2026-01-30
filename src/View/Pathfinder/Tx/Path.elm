module View.Pathfinder.Tx.Path exposing (inPath, inPathColored, inPathColoredHovered, inPathHovered, labelsSep, outPath, outPathColored, outPathColoredHovered, outPathHovered, pickPathFunction)

import Bezier
import Css
import Msg.Pathfinder exposing (Msg)
import String.Format as Format
import Svg.PathD exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Lazy as Svg
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Util.Graph exposing (translate)


labelsSep : String
labelsSep =
    "||"


inPath : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPath label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = False
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = False
        , color = Nothing
        }


inPathColored : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathColored color label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = False
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = False
        , color = Just color
        }


inPathHovered : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathHovered label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = False
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = False
        , color = Nothing
        }


inPathColoredHovered : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathColoredHovered color label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = False
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = False
        , color = Just color
        }


outPath : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPath label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = True
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = False
        , color = Nothing
        }


outPathHovered : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathHovered label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = True
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = False
        , color = Nothing
        }


outPathColored : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathColored color label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = True
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = False
        , color = Just color
        }


outPathColoredHovered : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathColoredHovered color label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = True
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = False
        , color = Just color
        }


inPathConversionLeg : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathConversionLeg label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = False
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = True
        , color = Nothing
        }


inPathColoredConversionLeg : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathColoredConversionLeg color label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = False
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = True
        , color = Just color
        }


inPathHoveredConversionLeg : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathHoveredConversionLeg label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = False
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = True
        , color = Nothing
        }


inPathColoredHoveredConversionLeg : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathColoredHoveredConversionLeg color label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = False
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = True
        , color = Just color
        }


outPathConversionLeg : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathConversionLeg label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = True
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = True
        , color = Nothing
        }


outPathHoveredConversionLeg : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathHoveredConversionLeg label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = True
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = True
        , color = Nothing
        }


outPathColoredConversionLeg : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathColoredConversionLeg color label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = True
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = True
        , color = Just color
        }


outPathColoredHoveredConversionLeg : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathColoredHoveredConversionLeg color label startX startY endX endY opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = True
        , x1 = startX
        , y1 = startY
        , x2 = endX
        , y2 = endY
        , opacity = opacity
        , isUtxo = True
        , dashed = True
        , color = Just color
        }


type alias ColoredPathConfig =
    { label : String
    , isOutgoing : Bool
    , highlight : Bool
    , x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    , opacity : Float
    , isUtxo : Bool
    , dashed : Bool
    , color : Maybe String
    }


arrowLength : Float
arrowLength =
    6


coloredPath : ColoredPathConfig -> Svg Msg
coloredPath c =
    let
        equals a b =
            a
                - b
                |> abs
                |> (>) 1.0e-6

        x2 =
            if equals c.x1 c.x2 then
                c.x2 + 0.01
                -- need to add this for the gradient to work

            else
                c.x2

        y2 =
            if equals c.y1 c.y2 then
                c.y2 + 0.01

            else
                c.y2

        ( dx, dy ) =
            ( x2 - c.x1
            , y2 - c.y1
            )

        ( val, _ ) =
            if c.isOutgoing then
                if dx < 0 then
                    ( GraphComponents.inputPathInputValue_details
                    , GraphComponents.inputPath_details
                    )

                else
                    ( GraphComponents.outputPathOutputValue_details
                    , GraphComponents.outputPath_details
                    )

            else if dx < 0 then
                ( GraphComponents.outputPathOutputValue_details
                , GraphComponents.outputPath_details
                )

            else
                ( GraphComponents.inputPathInputValue_details
                , GraphComponents.inputPath_details
                )

        { p0, p1, p2, p3 } =
            let
                ( mx, _ ) =
                    ( c.x1 + dx / 2
                    , c.y1 + dy / 2
                    )
            in
            { p0 = { x = c.x1, y = c.y1 }
            , p1 = { x = mx, y = c.y1 }
            , p2 = { x = mx, y = y2 }
            , p3 = { x = x2, y = y2 }
            }

        spline =
            if dx > 0 then
                Bezier.fromPoints p0 p1 p2 p3

            else
                Bezier.fromPoints p3 p2 p1 p0

        lx =
            p0.x + dx / 2

        {-
           if c.isOutgoing then
               if dx < 0 then
                   p3.x - (val.x / pa.width) * dx

               else
                   p3.x - (pa.width - val.x - val.width) / pa.width * dx

           else if dx < 0 then
               p0.x + ((val.x - val.width) / pa.width) * dx

           else
               p0.x + (val.x / pa.width) * dx
        -}
        ly =
            val.y
                + (Bezier.atX lx spline |> .point |> .y)

        p =
            [ M ( p0.x, p0.y )
            , C
                ( p1.x, p1.y )
                ( p2.x, p2.y )
                ( p3.x, p3.y )
            ]
                |> pathD
                |> d

        gradientStyles =
            c.color
                |> Maybe.withDefault
                    ("url(#{{ prefix }}{{ direction }}Edge{{ back }})"
                        |> Format.namedValue "prefix"
                            (if c.isUtxo then
                                "utxo"

                             else
                                "account"
                            )
                        |> Format.namedValue "direction"
                            (if c.isOutgoing then
                                "Out"

                             else
                                "In"
                            )
                        |> Format.namedValue "back"
                            (if dx > 0 then
                                "Forth"

                             else
                                "Back"
                            )
                    )
                |> Css.property "stroke"
                |> List.singleton

        path det isLine =
            Svg.path
                ([ p
                 , Css.property "stroke-width" (String.fromFloat det.strokeWidth)
                    :: (Css.property "fill" "none" |> Css.important)
                    :: det.styles
                    ++ gradientStyles
                    |> css
                 ]
                    ++ (if c.dashed && isLine then
                            [ strokeDasharray "5,5" ]

                        else
                            []
                       )
                )
                []

        boundingHeight =
            y2 - c.y1 |> abs

        strokeWidth =
            GraphComponents.outputPathHighlightLine_details.strokeWidth

        hackyRect =
            if boundingHeight < strokeWidth then
                let
                    rx =
                        if dx < 0 then
                            x2

                        else
                            c.x1

                    ry =
                        if dy < 0 then
                            y2

                        else
                            c.y1
                in
                rect
                    [ x <| String.fromFloat rx
                    , y <| String.fromFloat <| ry - strokeWidth / 2
                    , width <| String.fromFloat <| abs dx
                    , height <| String.fromFloat <| strokeWidth
                    , fill "transparent"
                    ]
                    []

            else
                g [] []
    in
    [ g
        (if c.highlight then
            [ Svg.Styled.Attributes.filter "url(#dropShadowEdgeHighlight)" ]

         else
            []
        )
        ((if c.highlight then
            [ path
                (if c.isOutgoing then
                    GraphComponents.outputPathHighlightLine_details

                 else
                    GraphComponents.inputPathHighlightLine_details
                )
                False
            , hackyRect
            ]

          else
            []
         )
            ++ [ path
                    (if c.isOutgoing then
                        GraphComponents.outputPathMainLine_details

                     else
                        GraphComponents.inputPathMainLine_details
                    )
                    True
               , if c.isOutgoing then
                    Svg.path
                        [ d <|
                            pathD
                                [ M ( x2 - arrowLength - 2, y2 - arrowLength * 0.7 )
                                , l ( arrowLength, arrowLength * 0.7 )
                                , l ( -arrowLength, arrowLength * 0.7 )
                                , Svg.PathD.z
                                ]
                        , "rotate("
                            ++ ([ if dx < 0 then
                                    "180"

                                  else
                                    "0"
                                , String.fromFloat x2
                                , String.fromFloat y2
                                ]
                                    |> String.join ","
                               )
                            ++ ")"
                            |> transform
                        , (c.color
                            |> Maybe.withDefault Colors.pathOut
                            |> Css.property "stroke"
                          )
                            :: (c.color
                                    |> Maybe.withDefault Colors.pathOut
                                    |> Css.property "fill"
                                    |> Css.important
                               )
                            :: GraphComponents.outputPathMainLine_details.styles
                            |> css
                        ]
                        []

                 else
                    text ""
               ]
        )
    , text_
        [ translate lx ly
            |> transform
        , textAnchor "middle"
        , dominantBaseline "hanging"
        , ([ Css.px 12 |> Css.fontSize
           , Css.property "fill" Colors.black0
           ]
            ++ (if c.highlight then
                    [ Css.property "stroke" Colors.greyBlue20
                    , Css.property "stroke-width" "6px"
                    , Css.property "paint-order" "stroke fill"
                    , Css.property "stroke-linejoin" "round"
                    ]

                else
                    []
               )
          )
            |> css
        ]
        (c.label
            |> String.split "||"
            |> List.indexedMap
                (\i ->
                    text
                        >> List.singleton
                        >> tspan
                            [ Svg.Styled.Attributes.x "0"
                            , Svg.Styled.Attributes.dy (String.fromInt i ++ "em")
                            ]
                )
        )
    ]
        |> g
            [ c.opacity |> String.fromFloat |> opacity
            , Css.cursor Css.pointer |> List.singleton |> css
            ]


pickPathFunction : Bool -> Bool -> Maybe String -> Bool -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
pickPathFunction isOutgoing hovered color isConversionLeg =
    case ( isOutgoing, hovered, ( color, isConversionLeg ) ) of
        ( False, False, ( Nothing, False ) ) ->
            Svg.lazy6 inPath

        ( False, True, ( Nothing, False ) ) ->
            Svg.lazy6 inPathHovered

        ( False, False, ( Just c, False ) ) ->
            Svg.lazy7 inPathColored c

        ( False, True, ( Just c, False ) ) ->
            Svg.lazy7 inPathColoredHovered c

        ( True, False, ( Nothing, False ) ) ->
            Svg.lazy6 outPath

        ( True, True, ( Nothing, False ) ) ->
            Svg.lazy6 outPathHovered

        ( True, False, ( Just c, False ) ) ->
            Svg.lazy7 outPathColored c

        ( True, True, ( Just c, False ) ) ->
            Svg.lazy7 outPathColoredHovered c

        ( False, False, ( Nothing, True ) ) ->
            Svg.lazy6 inPathConversionLeg

        ( False, True, ( Nothing, True ) ) ->
            Svg.lazy6 inPathHoveredConversionLeg

        ( False, False, ( Just c, True ) ) ->
            Svg.lazy7 inPathColoredConversionLeg c

        ( False, True, ( Just c, True ) ) ->
            Svg.lazy7 inPathColoredHoveredConversionLeg c

        ( True, False, ( Nothing, True ) ) ->
            Svg.lazy6 outPathConversionLeg

        ( True, True, ( Nothing, True ) ) ->
            Svg.lazy6 outPathHoveredConversionLeg

        ( True, False, ( Just c, True ) ) ->
            Svg.lazy7 outPathColoredConversionLeg c

        ( True, True, ( Just c, True ) ) ->
            Svg.lazy7 outPathColoredHoveredConversionLeg c
