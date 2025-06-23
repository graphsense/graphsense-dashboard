module View.Pathfinder.AggEdge exposing (edge, view)

import Config.View as View
import Css
import Init.Pathfinder.AggEdge as AggEdge
import Maybe.Extra
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter exposing (s_dividerLine, s_leftArrow, s_leftArrowGroup, s_leftValue, s_rectangleOfAggregatedLabel, s_rightArrow, s_rightArrowGroup, s_rightValue, s_root)
import RemoteData
import Svg.Styled exposing (Svg, g, line)
import Svg.Styled.Attributes as Svg exposing (css, transform, width)
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import Util.Graph exposing (translate)
import Util.View exposing (onClickWithStop)
import View.Locale as Locale
import View.Pathfinder.Tx.Utils exposing (toPosition)


type alias Dimensions =
    { x : Float
    , y : Float
    , leftLabelWidth : Float
    , rightLabelWidth : Float
    , leftLabel : String
    , rightLabel : String
    , leftValueRaw : Int
    , rightValueRaw : Int
    , offset : Float
    }


calcDimensions : View.Config -> AggEdge -> Address -> Address -> Dimensions
calcDimensions vc ed aAddress bAddress =
    let
        asset data =
            { network = data.address.currency, asset = data.address.currency }

        aPos =
            aAddress |> toPosition

        bPos =
            bAddress |> toPosition

        x =
            (aPos.x + bPos.x) / 2

        y =
            (aPos.y + bPos.y) / 2

        { leftRelation, rightRelation, aOffset } =
            if aPos.x < bPos.x then
                { leftRelation = ed.b2a
                , rightRelation = ed.a2b
                , aOffset = rad
                }

            else
                { leftRelation = ed.a2b
                , rightRelation = ed.b2a
                , aOffset = -rad
                }

        leftValue =
            relationToValue leftRelation

        rawValue =
            RemoteData.toMaybe
                >> Maybe.Extra.join
                >> Maybe.map (.value >> .value)
                >> Maybe.withDefault 0

        leftValueRaw =
            rawValue leftRelation

        rightValueRaw =
            rawValue rightRelation

        relationToValue =
            RemoteData.toMaybe
                >> Maybe.Extra.join
                >> Maybe.map
                    (\data ->
                        Locale.currencyWithoutCode vc.locale [ ( asset data, data.value ) ]
                    )
                >> Maybe.withDefault ""

        rightValue =
            relationToValue rightRelation

        charWidth =
            7.5

        labelWidthLeft =
            (String.length leftValue |> toFloat)
                * charWidth

        labelWidthRight =
            (String.length rightValue |> toFloat)
                * charWidth

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2
    in
    { x = x * unit
    , y = y * unit
    , leftLabelWidth = labelWidthLeft
    , rightLabelWidth = labelWidthRight
    , leftLabel = leftValue
    , rightLabel = rightValue
    , leftValueRaw = leftValueRaw
    , rightValueRaw = rightValueRaw
    , offset = aOffset
    }


view : View.Config -> AggEdge -> Address -> Address -> Svg Msg
view vc ed aAddress bAddress =
    let
        originalWidth =
            Theme.aggregatedLabelRectangleOfAggregatedLabel_details.width

        halfOriginalWidth =
            originalWidth / 2

        { leftLabelWidth, rightLabelWidth, x, y, leftLabel, rightLabel, leftValueRaw, rightValueRaw } =
            calcDimensions vc ed aAddress bAddress

        rectangleWidth =
            leftLabelWidth + rightLabelWidth

        hidden =
            css [ Css.opacity Css.zero |> Css.important ]

        corrH =
            -2.5
    in
    g
        []
        [ Theme.aggregatedLabelWithAttributes
            (Theme.aggregatedLabelAttributes
                |> s_root
                    [ translate
                        (x - rectangleWidth / 2)
                        (y - (Theme.aggregatedLabel_details.height / 2))
                        |> transform
                    , AggEdge.initId ed.a ed.b
                        |> UserClickedAggEdge
                        |> onClickWithStop
                    ]
                |> s_rectangleOfAggregatedLabel
                    [ width <| String.fromFloat rectangleWidth
                    ]
                |> s_rightArrowGroup
                    [ translate
                        (rectangleWidth - originalWidth)
                        0
                        |> transform
                    ]
                |> s_leftArrowGroup
                    [ translate
                        0
                        0
                        |> transform
                    ]
                |> s_leftArrow
                    (if leftValueRaw == 0 then
                        [ hidden ]

                     else
                        []
                    )
                |> s_rightArrow
                    (if rightValueRaw == 0 then
                        [ hidden ]

                     else
                        []
                    )
                |> s_dividerLine
                    (if leftValueRaw /= 0 && rightValueRaw /= 0 then
                        [ translate
                            (leftLabelWidth - halfOriginalWidth)
                            0
                            |> transform
                        ]

                     else
                        [ hidden ]
                    )
                |> s_rightValue
                    (if leftValueRaw == 0 then
                        [ translate
                            -halfOriginalWidth
                            corrH
                            |> transform
                        ]

                     else
                        [ translate
                            (rightLabelWidth - halfOriginalWidth)
                            corrH
                            |> transform
                        ]
                    )
                |> s_leftValue
                    (if rightValueRaw == 0 then
                        [ translate
                            (rectangleWidth - halfOriginalWidth)
                            corrH
                            |> transform
                        ]

                     else
                        [ translate
                            (leftLabelWidth - halfOriginalWidth)
                            corrH
                            |> transform
                        ]
                    )
            )
            { root =
                { leftValue = leftLabel
                , rightValue = rightLabel
                , showHighlight = False
                }
            }
        ]


edge : View.Config -> AggEdge -> Address -> Address -> Svg Msg
edge vc ed aAddress bAddress =
    let
        aPos =
            aAddress |> toPosition

        bPos =
            bAddress |> toPosition

        { offset } =
            calcDimensions vc ed aAddress bAddress

        fd =
            GraphComponents.addressNodeNodeFrame_details
    in
    g
        []
        [ line
            [ Svg.x1 <| String.fromFloat <| aPos.x * unit + offset
            , Svg.y1 <| String.fromFloat <| aPos.y * unit
            , Svg.x2 <| String.fromFloat <| bPos.x * unit - offset
            , Svg.y2 <| String.fromFloat <| bPos.y * unit
            , css Theme.aggregatedLinkMainLine_details.styles
            , css
                [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkMainLine_details.strokeWidth
                , Css.property "stroke" "black"
                ]
            ]
            []
        ]
