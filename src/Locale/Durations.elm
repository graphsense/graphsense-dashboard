module Locale.Durations exposing (Unit(..), durationToString)

import List.Extra
import Tuple exposing (second)


type alias Config =
    { unitToString : Int -> Unit -> String
    , separator : String
    , precision : Int
    }


type Unit
    = Seconds
    | Minutes
    | Hours
    | Days
    | Months
    | Years


durationToString : Config -> Int -> String
durationToString { unitToString, precision, separator } dur =
    toFloat dur
        / 1000
        |> abs
        |> (\d ->
                [ ( Years, 12 )
                , ( Months, 30.44 )
                , ( Days, 24 )
                , ( Hours, 60 )
                , ( Minutes, 60 )
                , ( Seconds, 1 )
                ]
                    |> List.foldl
                        (\( unit, base ) { found, range, rest } ->
                            { found =
                                ( unit
                                , rest
                                    / range
                                    |> floor
                                )
                                    :: found
                            , rest =
                                modBy (round range) (round rest)
                                    |> toFloat
                            , range = range / base
                            }
                        )
                        { range = 31560192
                        , rest = d
                        , found = []
                        }
                    |> .found
                    |> List.reverse
                    |> List.Extra.dropWhile (second >> (==) 0)
                    |> List.take precision
                    |> List.filter (second >> (/=) 0)
                    |> List.map
                        (\( unit, c ) ->
                            unitToString c unit
                        )
                    |> String.join separator
           )
