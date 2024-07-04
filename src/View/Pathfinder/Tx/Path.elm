module View.Pathfinder.Tx.Path exposing (accountPath, inPath, inPathHovered, outPath, outPathHovered)

import Config.Pathfinder as Pathfinder
import Config.View as View
import Css
import Css.Pathfinder as Css
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Tx exposing (..)
import Msg.Pathfinder exposing (Msg(..))
import String.Format as Format
import Svg.PathD exposing (..)
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Svg exposing (..)
import Svg.Styled.Lazy as Svg
import Theme.Svg.GraphComponents as GraphComponents exposing (defaultTxLabelAttributes)
import Util.Graph exposing (translate)
import Util.View


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


accountPath : View.Config -> String -> Float -> Float -> Float -> Float -> Float -> Svg Msg
accountPath vc label x1 y1 x2 y2 opacity =
    coloredPath vc
        { label = label
        , highlight = False
        , isOutgoing = True
        , x1 = x1
        , y1 = y1
        , x2 = x2
        , y2 = y2
        , opacity = opacity
        , isUtxo = False
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
    }


coloredPath : View.Config -> ColoredPathConfig -> Svg Msg
coloredPath vc c =
    let
        x2 =
            if c.x1 == c.x2 then
                c.x2 + 0.01

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

        ( val, pa ) =
            if c.isOutgoing then
                ( GraphComponents.outputPathOutputValueDetails
                , GraphComponents.outputPathDetails
                )

            else
                ( GraphComponents.inputPathInputValueDetails
                , GraphComponents.inputPathDetails
                )

        ( nodes, lx, ly ) =
            let
                ( mx, my ) =
                    ( c.x1 + dx / 2
                    , c.y1 + dy / 2
                    )
            in
            ( [ ( x2, y2 )
                    |> C
                        ( mx, c.y1 )
                        ( mx, c.y2 )
              ]
            , if c.isOutgoing then
                if dx < 0 then
                    x2 + val.x

                else
                    x2 - (pa.width - val.x - val.width)

              else if dx < 0 then
                c.x1 - val.x - val.width

              else
                c.x1 + val.x
            , if c.isOutgoing then
                c.y2 + val.y

              else
                c.y1 + val.y
            )
    in
    [ if c.highlight then
        let
            det =
                if c.isOutgoing then
                    GraphComponents.outputPathHighlightLineDetails

                else
                    GraphComponents.inputPathHighlightLineDetails
        in
        Svg.path
            [ nodes
                |> (::) (M ( c.x1, c.y1 ))
                |> pathD
                |> d
            , css det.styles
            ]
            []

      else
        g [] []
    , Svg.path
        [ nodes
            |> (::) (M ( c.x1, c.y1 ))
            |> pathD
            |> d
        , let
            det =
                if c.isOutgoing then
                    GraphComponents.outputPathMainLineDetails

                else
                    GraphComponents.inputPathMainLineDetails
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
        let
            arrowLength =
                vc.theme.pathfinder.arrowLength
        in
        Svg.path
            [ d <|
                pathD
                    [ M ( x2 - arrowLength, y2 - arrowLength )
                    , l ( arrowLength, arrowLength )
                    , l ( -arrowLength, arrowLength )
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
            , css
                (Css.edge vc
                    ++ [ Css.property "stroke" vc.theme.pathfinder.outEdgeColor ]
                )
            ]
            []

      else
        text ""
    , let
        lw =
            c.label
                |> String.length
                |> toFloat
                |> (*) (GraphComponents.txLabelLabelDetails.width / 3)

        fr =
            GraphComponents.txLabelRectangleDetails
      in
      tspan
        [ alignmentBaseline "hanging"
        ]
        [ text c.label ]
        |> List.singleton
        |> text_
            [ translate lx ly
                |> transform
            , if c.isOutgoing then
                textAnchor "end"

              else
                textAnchor "start"
            ]
    ]
        |> g
            [ c.opacity |> String.fromFloat |> opacity
            , Css.cursor Css.pointer |> List.singleton |> css
            ]


bendedPath : View.Config -> Pathfinder.Config -> String -> Bool -> Float -> Float -> Float -> Float -> Svg Msg
bendedPath vc _ label withArrow x1 y1 x2 y2 =
    let
        ( dx, dy ) =
            ( x2 - x1
            , y2 - y1
            )

        ( nodes, lx, ly ) =
            if dx > 0 then
                let
                    ( mx, my ) =
                        ( x1 + dx / 2
                        , y1 + dy / 2
                        )
                in
                ( [ ( x2, y2 )
                        |> C
                            ( mx, y1 )
                            ( mx, y2 )
                  ]
                , mx
                , my
                )

            else
                let
                    ( mx, my ) =
                        ( x1 + dx / 2
                        , y1 + Basics.max (dy / 2) vc.theme.pathfinder.addressRadius
                        )

                    ( c1x, c1y ) =
                        ( x1 + (mx - x1 |> abs) / 2
                        , my
                        )

                    ( c2x, c2y ) =
                        ( mx
                            - (mx - x1)
                            / 2
                        , my
                        )
                in
                ( [ ( mx, my )
                        |> C
                            ( c1x, c1y )
                            ( c2x, c2y )
                  , ( x2, y2 )
                        |> S
                            ( x2 - (x2 - mx |> abs) / 2
                            , y2 - (y2 - my) / 2
                            )
                  ]
                , mx
                , my
                )
    in
    [ Svg.path
        [ nodes
            |> (::) (M ( x1, y1 ))
            |> pathD
            |> d
        , Css.edge vc |> css
        ]
        []
    , if withArrow then
        let
            arrowLength =
                vc.theme.pathfinder.arrowLength
        in
        Svg.path
            [ d <|
                pathD
                    [ M ( x2 - arrowLength, y2 - arrowLength )
                    , l ( arrowLength, arrowLength )
                    , l ( -arrowLength, arrowLength )
                    ]
            , Css.edge vc |> css
            ]
            []

      else
        text ""
    , text_
        [ lx |> String.fromFloat |> x
        , ly |> String.fromFloat |> y
        , textAnchor "middle"
        , Css.edgeLabel vc |> css
        ]
        [ text label
        ]
    ]
        |> g []
