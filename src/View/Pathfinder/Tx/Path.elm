module View.Pathfinder.Tx.Path exposing (inPath, inPathColored, inPathColoredHovered, inPathHovered, outPath, outPathColored, outPathColoredHovered, outPathHovered, pickPathFunction)

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


inPath : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPath label x1 y1 x2 y2 opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = False
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        , color = Nothing
        }


inPathColored : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathColored color label x1 y1 x2 y2 opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = False
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        , color = Just color
        }


inPathHovered : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathHovered label x1 y1 x2 y2 opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = False
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        , color = Nothing
        }


inPathColoredHovered : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathColoredHovered color label x1 y1 x2 y2 opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = False
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        , color = Just color
        }


outPath : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPath label x1 y1 x2 y2 opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = True
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        , color = Nothing
        }


outPathHovered : String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathHovered label x1 y1 x2 y2 opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = True
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        , color = Nothing
        }


outPathColored : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathColored color label x1 y1 x2 y2 opacity =
    coloredPath
        { label = label
        , highlight = False
        , isOutgoing = True
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        , color = Just color
        }


outPathColoredHovered : String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathColoredHovered color label x1 y1 x2 y2 opacity =
    coloredPath
        { label = label
        , highlight = True
        , isOutgoing = True
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        , color = Just color
        }



-- accountPath : View.Config -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
-- accountPath vc label x1 y1 x2 y2 opacity =
--     coloredPath
--         { label = label
--         , highlight = False
--         , isOutgoing = True
--         , x1 = x1
--         , y1 = y1
--         , x2 = x2
--         , y2 = y2
--         , opacity = opacity
--         , isUtxo = False
--         }


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

        path det =
            Svg.path
                [ p
                , Css.property "stroke-width" (String.fromFloat det.strokeWidth)
                    :: det.styles
                    ++ gradientStyles
                    |> css
                ]
                []
    in
    [ if c.highlight then
        path
            (if c.isOutgoing then
                GraphComponents.outputPathHighlightLine_details

             else
                GraphComponents.inputPathHighlightLine_details
            )

      else
        g [] []
    , path
        (if c.isOutgoing then
            GraphComponents.outputPathMainLine_details

         else
            GraphComponents.inputPathMainLine_details
        )
    , if c.isOutgoing then
        Svg.path
            [ d <|
                pathD
                    [ M ( x2 - arrowLength, y2 - arrowLength * 0.7 )
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
    , text_
        [ translate lx ly
            |> transform

        {- , if c.isOutgoing then
             if dx > 0 then
                 textAnchor "end"

             else
                 textAnchor "start"

           else if dx > 0 then
             textAnchor "start"

           else
             textAnchor "end"
        -}
        , textAnchor "middle"
        , dominantBaseline "hanging"
        , [ Css.px 12 |> Css.fontSize
          , Css.property "fill" Colors.black0
          ]
            |> css

        -- fix font size to ensure scaling in export (screenshot)
        ]
        [ text c.label ]
    ]
        |> g
            [ c.opacity |> String.fromFloat |> opacity
            , Css.cursor Css.pointer |> List.singleton |> css
            ]


pickPathFunction : Bool -> Bool -> Maybe String -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
pickPathFunction isOutgoing hovered color =
    case ( isOutgoing, hovered, color ) of
        ( False, False, Nothing ) ->
            Svg.lazy6 inPath

        ( False, True, Nothing ) ->
            Svg.lazy6 inPathHovered

        ( False, False, Just c ) ->
            Svg.lazy7 inPathColored c

        ( False, True, Just c ) ->
            Svg.lazy7 inPathColoredHovered c

        ( True, False, Nothing ) ->
            Svg.lazy6 outPath

        ( True, True, Nothing ) ->
            Svg.lazy6 outPathHovered

        ( True, False, Just c ) ->
            Svg.lazy7 outPathColored c

        ( True, True, Just c ) ->
            Svg.lazy7 outPathColoredHovered c
