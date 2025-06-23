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


view : View.Config -> AggEdge -> Address -> Address -> Svg Msg
view vc ed aAddress bAddress =
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

        { leftRelation, rightRelation } =
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

        originalWidth =
            Theme.aggregatedLabelRectangleOfAggregatedLabel_details.width

        halfOriginalWidth =
            originalWidth / 2

        labelWidthLeft =
            (String.length leftValue |> toFloat)
                * charWidth

        labelWidthRight =
            (String.length rightValue |> toFloat)
                * charWidth

        rectangleWidth =
            labelWidthLeft + labelWidthRight

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2

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
                        (x * unit - rectangleWidth / 2)
                        (y * unit - (Theme.aggregatedLabel_details.height / 2))
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
                            (labelWidthLeft - halfOriginalWidth)
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
                            (labelWidthLeft - halfOriginalWidth)
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
                            (labelWidthLeft - halfOriginalWidth)
                            corrH
                            |> transform
                        ]
                    )
            )
            { root =
                { leftValue = leftValue
                , rightValue = rightValue
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

        aOffset =
            if aPos.x < bPos.x then
                rad

            else
                -rad

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2
    in
    g
        []
        [ line
            [ Svg.x1 <| String.fromFloat <| aPos.x * unit + aOffset
            , Svg.y1 <| String.fromFloat <| aPos.y * unit
            , Svg.x2 <| String.fromFloat <| bPos.x * unit - aOffset
            , Svg.y2 <| String.fromFloat <| bPos.y * unit
            , css Theme.aggregatedLinkMainLine_details.styles
            , css
                [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkMainLine_details.strokeWidth
                , Css.property "stroke" "black"
                ]
            ]
            []
        ]
