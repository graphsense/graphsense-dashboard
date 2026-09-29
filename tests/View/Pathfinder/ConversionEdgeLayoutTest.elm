module View.Pathfinder.ConversionEdgeLayoutTest exposing (suite)

{-| A swap edge is a curve from its input side to its output side with the swap
icon on it. The icon can be dragged: the curve must then still pass through the
icon, and without a drag the icon stays where it always was.
-}

import Expect exposing (FloatingPointTolerance(..))
import Test exposing (Test, describe, test)
import View.Pathfinder.ConversionEdge as ConversionEdge exposing (Curve(..))


start : ( Float, Float )
start =
    ( 100, 50 )


end : ( Float, Float )
end =
    ( 120, 400 )


noOffset : { x : Float, y : Float }
noOffset =
    { x = 0, y = 0 }


{-| The point of a cubic Bézier at t = 0.5.
-}
midpoint : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
midpoint ( x0, y0 ) ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) =
    ( (x0 + 3 * x1 + 3 * x2 + x3) / 8, (y0 + 3 * y1 + 3 * y2 + y3) / 8 )


expectPoint : ( Float, Float ) -> ( Float, Float ) -> Expect.Expectation
expectPoint ( ex, ey ) ( ax, ay ) =
    Expect.all
        [ \_ -> ax |> Expect.within (Absolute 1.0e-6) ex
        , \_ -> ay |> Expect.within (Absolute 1.0e-6) ey
        ]
        ()


{-| The icon checked against the point of its own curve at t = 0.5.
-}
expectOnCurve : ConversionEdge.Layout -> Expect.Expectation
expectOnCurve l =
    case l.curve of
        Bezier c1 c2 ->
            l.node |> expectPoint (midpoint start c1 c2 end)

        Loop _ ->
            Expect.fail "two different nodes must draw a curve, not a loop"


suite : Test
suite =
    describe "swap edge layout"
        [ test "an unmoved icon sits on the middle of its curve" <|
            \_ ->
                let
                    l =
                        ConversionEdge.layout { start = start, end = end, displacementIndex = 0, nodeOffset = noOffset }
                in
                expectOnCurve l
        , test "an unmoved icon is where it always was" <|
            \_ ->
                -- controls extend 150 to the right of both ends
                ConversionEdge.layout { start = start, end = end, displacementIndex = 0, nodeOffset = noOffset }
                    |> .node
                    |> expectPoint (midpoint start ( 250, 50 ) ( 270, 400 ) end)
        , test "a moved icon is where it was dropped" <|
            \_ ->
                let
                    before =
                        ConversionEdge.layout { start = start, end = end, displacementIndex = 0, nodeOffset = noOffset }

                    after =
                        ConversionEdge.layout { start = start, end = end, displacementIndex = 0, nodeOffset = { x = 60, y = -35 } }
                in
                after.node |> expectPoint ( Tuple.first before.node + 60, Tuple.second before.node - 35 )
        , test "the curve follows a moved icon" <|
            \_ ->
                let
                    l =
                        ConversionEdge.layout { start = start, end = end, displacementIndex = 0, nodeOffset = { x = 60, y = -35 } }
                in
                expectOnCurve l
        , test "a moved loop icon is where it was dropped" <|
            \_ ->
                let
                    before =
                        ConversionEdge.layout { start = start, end = start, displacementIndex = 0, nodeOffset = noOffset }

                    after =
                        ConversionEdge.layout { start = start, end = start, displacementIndex = 0, nodeOffset = { x = -20, y = 45 } }
                in
                after.node |> expectPoint ( Tuple.first before.node - 20, Tuple.second before.node + 45 )
        , test "the loop's tip follows a moved icon" <|
            \_ ->
                let
                    l =
                        ConversionEdge.layout { start = start, end = start, displacementIndex = 0, nodeOffset = { x = -20, y = 45 } }
                in
                case l.curve of
                    Loop { tip } ->
                        l.node |> expectPoint tip

                    Bezier _ _ ->
                        Expect.fail "same start and end must draw a loop"
        ]
