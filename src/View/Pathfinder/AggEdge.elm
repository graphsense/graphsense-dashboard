module View.Pathfinder.AggEdge exposing (view)

import Config.View as View
import Css
import Init.Pathfinder.AggEdge as AggEdge
import Maybe.Extra
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AggEdge exposing (AggEdge)
import Model.Pathfinder.Tx exposing (TxType(..))
import Msg.Pathfinder exposing (Msg(..))
import RecordSetter exposing (s_leftArrowGroup, s_rectangleOfAggregatedLabel, s_rightArrowGroup, s_root)
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
view vc edge aAddress bAddress =
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
                { leftRelation = edge.b2a
                , rightRelation = edge.a2b
                , aOffset = rad
                }

            else
                { leftRelation = edge.a2b
                , rightRelation = edge.b2a
                , aOffset = -rad
                }

        leftValue =
            relationToValue leftRelation

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
            8

        halfOriginalWidth =
            Theme.aggregatedLabelRectangleOfAggregatedLabel_details.width / 2

        mx =
            toFloat >> max halfOriginalWidth

        labelWidthLeft =
            String.length leftValue
                * charWidth
                |> mx

        labelWidthRight =
            String.length rightValue
                * charWidth
                |> mx

        rectangleWidth =
            labelWidthLeft + labelWidthRight

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
        , Theme.aggregatedLabelWithAttributes
            (Theme.aggregatedLabelAttributes
                |> s_root
                    [ translate
                        (x * unit - labelWidthLeft)
                        (y * unit - (Theme.aggregatedLabel_details.height / 2))
                        |> transform
                    , AggEdge.initId edge.a edge.b
                        |> UserClickedAggEdge
                        |> onClickWithStop
                    ]
                |> s_rectangleOfAggregatedLabel
                    [ width <| String.fromFloat rectangleWidth
                    ]
                |> s_rightArrowGroup
                    [ translate
                        (labelWidthRight - halfOriginalWidth)
                        0
                        |> transform
                    ]
                |> s_leftArrowGroup
                    [ translate
                        -(labelWidthLeft - halfOriginalWidth)
                        0
                        |> transform
                    ]
            )
            { root =
                { leftValue = leftValue
                , rightValue = rightValue
                , showHighlight = False
                }
            }
        ]
