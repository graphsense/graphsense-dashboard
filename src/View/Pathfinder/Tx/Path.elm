module View.Pathfinder.Tx.Path exposing (inPath, inPathHovered, outPath, outPathHovered)

import Bezier
import Color
import Config.View as View
import Css
import Msg.Pathfinder exposing (Msg)
import String.Format as Format
import Svg.PathD exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Util.Graph exposing (translate)


inPath : View.Config -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPath vc label x1 y1 x2 y2 opacity =
    coloredPath vc
        { label = label
        , highlight = False
        , isOutgoing = False
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        }


inPathHovered : View.Config -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
inPathHovered vc label x1 y1 x2 y2 opacity =
    coloredPath vc
        { label = label
        , highlight = True
        , isOutgoing = False
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        }


outPath : View.Config -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPath vc label x1 y1 x2 y2 opacity =
    coloredPath vc
        { label = label
        , highlight = False
        , isOutgoing = True
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        }


outPathHovered : View.Config -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
outPathHovered vc label x1 y1 x2 y2 opacity =
    coloredPath vc
        { label = label
        , highlight = True
        , isOutgoing = True
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = True
        }



-- accountPath : View.Config -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
-- accountPath vc label x1 y1 x2 y2 opacity =
--     coloredPath vc
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
    }


arrowLength : Float
arrowLength =
    6


coloredPath : View.Config -> ColoredPathConfig -> Svg Msg
coloredPath vc c =
    let
        x2 =
            if c.x1 == c.x2 then
                c.x2 + 0.01
                -- need to add this for the gradient to work

            else
                c.x2

        y2 =
            if c.y1 == c.y2 then
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
    in
    [ if c.highlight then
        let
            det =
                if c.isOutgoing then
                    GraphComponents.outputPathHighlightLine_details

                else
                    GraphComponents.inputPathHighlightLine_details
        in
        Svg.path
            [ p
            , css det.styles
            ]
            []

      else
        g [] []
    , Svg.path
        [ p
        , let
            det =
                if c.isOutgoing then
                    GraphComponents.outputPathMainLine_details

                else
                    GraphComponents.inputPathMainLine_details
          in
          det.styles
            ++ [ "url(#{{ prefix }}{{ direction }}Edge{{ back }})"
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
                    |> Css.property "stroke"
               ]
            |> css
        ]
        []
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
            , (Colors.pathOut
                |> Css.property "stroke"
              )
                :: (Colors.pathOut
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
        ]
        [ text c.label ]
    ]
        |> g
            [ c.opacity |> String.fromFloat |> opacity
            , Css.cursor Css.pointer |> List.singleton |> css
            ]



-- bendedPath : View.Config -> Pathfinder.Config -> String -> Bool -> Float -> Float -> Float -> Float -> Svg Msg
-- bendedPath vc _ label withArrow x1 y1 x2 y2 =
--     let
--         ( dx, dy ) =
--             ( x2 - x1
--             , y2 - y1
--             )
--         ( nodes, lx, ly ) =
--             if dx > 0 then
--                 let
--                     ( mx, my ) =
--                         ( x1 + dx / 2
--                         , y1 + dy / 2
--                         )
--                 in
--                 ( [ ( x2, y2 )
--                         |> C
--                             ( mx, y1 )
--                             ( mx, y2 )
--                   ]
--                 , mx
--                 , my
--                 )
--             else
--                 let
--                     ( mx, my ) =
--                         ( x1 + dx / 2
--                         , y1 + Basics.max (dy / 2) (GraphComponents.addressNodeNodeFrame_details.width / 2)
--                         )
--                     ( c1x, c1y ) =
--                         ( x1 + (mx - x1 |> abs) / 2
--                         , my
--                         )
--                     ( c2x, c2y ) =
--                         ( mx
--                             - (mx - x1)
--                             / 2
--                         , my
--                         )
--                 in
--                 ( [ ( mx, my )
--                         |> C
--                             ( c1x, c1y )
--                             ( c2x, c2y )
--                   , ( x2, y2 )
--                         |> S
--                             ( x2 - (x2 - mx |> abs) / 2
--                             , y2 - (y2 - my) / 2
--                             )
--                   ]
--                 , mx
--                 , my
--                 )
--     in
--     [ Svg.path
--         [ nodes
--             |> (::) (M ( x1, y1 ))
--             |> pathD
--             |> d
--         ]
--         []
--     , if withArrow then
--         Svg.path
--             [ d <|
--                 pathD
--                     [ M ( x2 - arrowLength, y2 - arrowLength )
--                     , l ( arrowLength, arrowLength )
--                     , l ( -arrowLength, arrowLength )
--                     ]
--             ]
--             []
--       else
--         text ""
--     , text_
--         [ lx |> String.fromFloat |> x
--         , ly |> String.fromFloat |> y
--         , textAnchor "middle"
--         ]
--         [ text label
--         ]
--     ]
--         |> g []
