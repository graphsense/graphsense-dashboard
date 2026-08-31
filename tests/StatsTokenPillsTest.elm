module StatsTokenPillsTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import View.Stats exposing (cappedTokenPills)


tickers : Int -> List String
tickers n =
    List.range 1 n |> List.map (\i -> "T" ++ String.fromInt i)


suite : Test
suite =
    describe "cappedTokenPills"
        [ test "ten or fewer tickers all get a pill" <|
            \_ ->
                cappedTokenPills (tickers 10)
                    |> Expect.equal (tickers 10)
        , test "above ten, the tenth pill counts the rest" <|
            \_ ->
                cappedTokenPills (tickers 14)
                    |> Expect.equal (tickers 9 ++ [ "5+" ])
        , test "empty stays empty" <|
            \_ ->
                cappedTokenPills []
                    |> Expect.equal []
        ]
