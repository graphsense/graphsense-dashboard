module Locale.TestDurations exposing (suite)

import Expect
import Locale.Durations as Durations
import Test


data : List ( Float, Int, String )
data =
    [ ( 0, 3, "" )
    , ( 12, 3, "12 Seconds" )
    , ( 12 * 60, 3, "12 Minutes" )
    , ( 12 * 60 + 1, 3, "12 Minutes 1 Seconds" )
    , ( 60 * 60 * 3 + 1, 3, "3 Hours 1 Seconds" )
    , ( 60 * 60 * 3 + 71, 3, "3 Hours 1 Minutes 11 Seconds" )
    , ( 60 * 60 * 3 + 191, 3, "3 Hours 3 Minutes 11 Seconds" )
    , ( 60 * 60 * 3 * 24 + 191, 4, "3 Days 3 Minutes 11 Seconds" )
    , ( 60 * 60 * 3 * 24 + 191, 3, "3 Days 3 Minutes" )
    , ( 60 * 60 * 3 * 24 + 3791, 4, "3 Days 1 Hours 3 Minutes 11 Seconds" )
    , ( 60 * 60 * 3 * 24 + 3791, 3, "3 Days 1 Hours 3 Minutes" )
    , ( 60 * 60 * 3 * 24 * 30.44 + 24 * 60 * 60 * 9 + 3791, 5, "3 Months 9 Days 1 Hours 3 Minutes 11 Seconds" )
    , ( 60 * 60 * 3 * 24 * 30.44 + 24 * 60 * 60 * 9 + 3791, 4, "3 Months 9 Days 1 Hours 3 Minutes" )
    , ( 60 * 60 * 3 * 24 * 30.44 + 24 * 60 * 60 * 9 + 3791, 3, "3 Months 9 Days 1 Hours" )
    , ( 60 * 60 * 3 * 24 * 30.44 + 24 * 60 * 60 * 9 + 3791, 2, "3 Months 9 Days" )
    , ( 60 * 60 * 3 * 24 * 30.44 + 24 * 60 * 60 * 9 + 3791, 1, "3 Months" )
    , ( 60 * 60 * 3 * 24 * 30.44 * 13 + 3791, 6, "3 Years 3 Months 1 Hours 3 Minutes 11 Seconds" )
    , ( 60 * 60 * 3 * 24 * 30.44 * 13 + 3791, 3, "3 Years 3 Months" )
    , ( -60 * 60 * 24 * 30.44 * 12, 3, "1 Years" )
    ]


config : { unitToString : Int -> x -> String, separator : String, precision : Int }
config =
    { unitToString = \i u -> String.fromInt i ++ " " ++ Debug.toString u
    , separator = " "
    , precision = 3
    }


suite : Test.Test
suite =
    Test.describe "The Durations module"
        (data
            |> List.map
                (\( d, precision, result ) ->
                    Test.test ("test: " ++ result) <|
                        \_ ->
                            Durations.durationToString
                                { config | precision = precision }
                                (round <| d * 1000)
                                |> Expect.equal result
                )
        )
