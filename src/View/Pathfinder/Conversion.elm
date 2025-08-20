module View.Pathfinder.Conversion exposing (edge)

import Config.View as View
import Css
import Model.Pathfinder exposing (unit)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.Conversion exposing (Conversion)
import Msg.Pathfinder exposing (Msg)
import Svg.PathD exposing (Segment(..), pathD)
import Svg.Styled exposing (Svg, g, path)
import Svg.Styled.Attributes as Svg exposing (css)
import Theme.Colors as Colors
import Theme.Svg.GraphComponents as GraphComponents
import Theme.Svg.GraphComponentsAggregatedTracing as Theme
import Util.TextDimensions as TextDimensions
import View.Pathfinder.Tx.Utils exposing (Pos, toPosition)



-- view : Plugins -> View.Config -> Pathfinder.Config -> ExternalConversion -> Address -> Address-> Svg Msg
-- view plugins vc gc tx conversion =
--     Svg.g [] []


type alias Dimensions =
    { x : Float
    , y : Float
    , totalWidth : Float
    , leftLabelWidth : Float
    , rightLabelWidth : Float
    , leftLabel : String
    , rightLabel : String
    , leftVisible : Bool
    , rightVisible : Bool
    , left : Pos
    , right : Pos
    }


calcDimensions : View.Config -> Conversion -> Address -> Address -> Dimensions
calcDimensions vc _ aAddress bAddress =
    let
        padding =
            15

        -- Padding for the labels
        aPos =
            aAddress |> toPosition

        bPos =
            bAddress |> toPosition

        x =
            (aPos.x + bPos.x) / 2

        y =
            (aPos.y + bPos.y) / 2

        { left, right } =
            if aPos.x < bPos.x then
                { left = aPos
                , right = bPos
                }

            else
                { left = bPos
                , right = aPos
                }

        leftLabel =
            "test 2"

        leftValueVisible =
            not (String.isEmpty leftLabel)

        rightValueVisible =
            not (String.isEmpty rightLabel)

        rightLabel =
            "test"

        leftLabelWidth =
            TextDimensions.estimateTextWidth vc.characterDimensions leftLabel
                + (if String.isEmpty leftLabel then
                    0

                   else
                    padding
                  )

        rightLabelWidth =
            TextDimensions.estimateTextWidth vc.characterDimensions rightLabel
                + (if String.isEmpty rightLabel then
                    0

                   else
                    padding
                  )

        totalWidth =
            Theme.aggregatedLabel_details.width
                - Theme.aggregatedLabelRectangleOfAggregatedLabel_details.width
                + leftLabelWidth
                + rightLabelWidth
    in
    { x = x * unit
    , y = y * unit
    , totalWidth = totalWidth
    , leftLabelWidth = leftLabelWidth
    , rightLabelWidth = rightLabelWidth
    , leftLabel = leftLabel
    , rightLabel = rightLabel
    , leftVisible = leftValueVisible
    , rightVisible = rightValueVisible
    , left = left
    , right = right
    }


edge : View.Config -> Conversion -> Address -> Address -> Svg Msg
edge vc conversion inputAddress outputAddress =
    let
        { left, right, totalWidth, x, y } =
            calcDimensions vc conversion inputAddress outputAddress

        fd =
            GraphComponents.addressNodeNodeFrame_details

        rad =
            fd.width / 2 + fd.strokeWidth

        ax_ =
            left.x * unit + rad

        ax =
            if ax_ > lx then
                ax_ - 2 * rad

            else
                ax_

        ay =
            left.y * unit

        bx_ =
            right.x * unit - rad

        bx =
            if bx_ < rx then
                bx_ + 2 * rad

            else
                bx_

        by =
            right.y * unit

        lx =
            x - totalWidth / 2

        rx =
            x + totalWidth / 2

        maxDiff =
            5

        diffr =
            bx
                - rx
                |> min 0

        diffrCap =
            diffr
                |> max (negate maxDiff)

        pat =
            pathD
                [ M ( ax, ay )

                -- , C ( ax + (lx - ax) / 3 - diffl, y ) ( lx - difflCap, y ) ( lx, y )
                -- , L ( rx, y )
                , C ( rx - diffrCap, y ) ( rx + (bx - rx) / 3 * 2 - diffr, y ) ( bx, by )
                ]

        hl =
            conversion.hovered
    in
    g
        [--     id
         --     |> UserClickedAggEdge
         --     |> onClickWithStop
         -- , id
         --     |> UserMovesMouseOutAggEdge
         --     |> onMouseLeave
         -- , id
         --     |> UserMovesMouseOverAggEdge
         --     |> onMouseOver
        ]
        [ path
            [ Svg.d pat
            , css Theme.aggregatedLinkHighlightLine_details.styles
            , css
                [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkHighlightLine_details.strokeWidth
                , Css.property "stroke" <|
                    if hl then
                        Colors.pathAggregatedHighlight

                    else
                        "transparent"
                , Css.property "fill" "none" |> Css.important
                , Css.property "stroke-linecap" "square"
                ]
            ]
            []
        , path
            [ Svg.d pat
            , css Theme.aggregatedLinkMainLine_details.styles
            , css
                [ Css.property "stroke-width" <| String.fromFloat Theme.aggregatedLinkMainLine_details.strokeWidth
                , Css.property "stroke" Colors.pathMiddle
                , Css.property "fill" "none" |> Css.important
                , Css.property "stroke-linecap" "square"
                ]
            ]
            []
        ]
