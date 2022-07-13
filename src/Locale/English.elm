module Locale.English exposing (unitToString)

import Locale.Durations exposing (Unit(..))


unitToString : Int -> Unit -> String
unitToString i unit =
    let
        ( singular, plural ) =
            case unit of
                Seconds ->
                    ( "second", "seconds" )

                Minutes ->
                    ( "minute", "minutes" )

                Hours ->
                    ( "hour", "hours" )

                Days ->
                    ( "day", "days" )

                Months ->
                    ( "month", "months" )

                Years ->
                    ( "year", "years" )
    in
    String.fromInt i
        ++ " "
        ++ (if i == 1 then
                singular

            else
                plural
           )
