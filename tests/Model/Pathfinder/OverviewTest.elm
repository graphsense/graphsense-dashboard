module Model.Pathfinder.OverviewTest exposing (suite)

import Expect exposing (FloatingPointTolerance(..))
import Model.Pathfinder.Overview as Overview exposing (Visibility(..))
import Test exposing (Test, describe, test)


points : List { x : Float, y : Float }
points =
    [ { x = 0, y = 0 }, { x = 1000, y = 200 }, { x = 400, y = -300 } ]


viewport : { x : Float, y : Float, width : Float, height : Float }
viewport =
    { x = -100, y = -100, width = 600, height = 400 }


shown : Visibility -> List { x : Float, y : Float } -> Bool
shown visibility =
    Overview.isShown 0 { visibility = visibility, dragging = False } viewport


suite : Test
suite =
    describe "Model.Pathfinder.Overview"
        [ test "every point lands on the map" <|
            \_ ->
                case Overview.layout 10 points of
                    Just l ->
                        points
                            |> List.map (Overview.toMap l)
                            |> List.all (\p -> p.x >= 0 && p.x <= Overview.mapWidth && p.y >= 0 && p.y <= Overview.mapHeight)
                            |> Expect.equal True

                    Nothing ->
                        Expect.fail "no layout for three points"
        , test "map and graph coordinates convert back and forth" <|
            \_ ->
                case Overview.layout 10 points of
                    Just l ->
                        Overview.toGraph l (Overview.toMap l { x = 1000, y = 200 })
                            |> (\p -> p.x + p.y)
                            |> Expect.within (Absolute 0.001) 1200

                    Nothing ->
                        Expect.fail "no layout for three points"
        , test "a single point still gets a layout" <|
            \_ -> Overview.layout 10 [ { x = 5, y = 5 } ] |> Expect.notEqual Nothing
        , test "no points, no layout" <|
            \_ -> Overview.layout 10 [] |> Expect.equal Nothing
        , describe "when it shows"
            [ test "not while the whole graph is on screen" <|
                \_ -> shown Auto [ { x = 0, y = 0 }, { x = 400, y = 200 } ] |> Expect.equal False
            , test "as soon as part of it is off screen" <|
                \_ -> shown Auto points |> Expect.equal True
            , test "never once hidden" <|
                \_ -> shown Hidden points |> Expect.equal False
            , test "always once brought back" <|
                \_ -> shown Shown [ { x = 0, y = 0 } ] |> Expect.equal True
            , test "never for an empty graph" <|
                \_ -> shown Shown [] |> Expect.equal False
            ]
        ]
