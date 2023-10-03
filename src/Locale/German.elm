module Locale.German exposing (german, relativeTimeOptions, unitToString)

import DateFormat.Language exposing (Language)
import DateFormat.Relative exposing (RelativeTimeOptions)
import Locale.Durations exposing (Unit(..))
import Time exposing (Month(..), Weekday(..))



-- German


{-| The German language!
-}
german : Language
german =
    Language
        toGermanMonthName
        toGermanMonthAbbreviation
        toGermanWeekdayName
        (toGermanWeekdayName >> String.left 2)
        (always "")
        (always "")


toGermanMonthName : Month -> String
toGermanMonthName month =
    case month of
        Jan ->
            "Januar"

        Feb ->
            "Februar"

        Mar ->
            "März"

        Apr ->
            "April"

        May ->
            "Mai"

        Jun ->
            "Juni"

        Jul ->
            "Juli"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "Oktober"

        Nov ->
            "November"

        Dec ->
            "Dezember"


toGermanMonthAbbreviation : Month -> String
toGermanMonthAbbreviation month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mär"

        Apr ->
            "Apr"

        May ->
            "Mai"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Okt"

        Nov ->
            "Nov"

        Dec ->
            "Dez"


toGermanWeekdayName : Weekday -> String
toGermanWeekdayName weekday =
    case weekday of
        Mon ->
            "Montag"

        Tue ->
            "Dienstag"

        Wed ->
            "Mittwoch"

        Thu ->
            "Donnerstag"

        Fri ->
            "Freitag"

        Sat ->
            "Samstag"

        Sun ->
            "Sonntag"


relative : String -> String -> String -> Int -> String
relative pronoun singular plural t =
    pronoun
        ++ " "
        ++ String.fromInt t
        ++ " "
        ++ (if t == 1 then
                singular

            else
                plural
           )


past : String -> String -> Int -> String
past =
    relative "vor"


relativeTimeOptions : RelativeTimeOptions
relativeTimeOptions =
    { someSecondsAgo = past "Sekunde" "Sekunden"
    , someMinutesAgo = past "Minute" "Minuten"
    , someHoursAgo = past "Stunde" "Stunden"
    , someDaysAgo = past "Tag" "Tagen"
    , someMonthsAgo = past "Monat" "Monaten"
    , someYearsAgo = past "Jahr" "Jahren"
    , rightNow = "jetzt"
    , inSomeSeconds = past "Sekunde" "Sekunden"
    , inSomeMinutes = past "MinSomeute" "MinSomeuten"
    , inSomeHours = past "Stunde" "Stunden"
    , inSomeDays = past "Tag" "Tagen"
    , inSomeMonths = past "Monat" "Monaten"
    , inSomeYears = past "Jahr" "Jahren"
    }


unitToString : Int -> Unit -> String
unitToString i unit =
    let
        ( singular, plural ) =
            case unit of
                Seconds ->
                    ( "Sekunde", "Sekunden" )

                Minutes ->
                    ( "Minute", "Minuten" )

                Hours ->
                    ( "Stunde", "Stunden" )

                Days ->
                    ( "Tag", "Tage" )

                Months ->
                    ( "Monat", "Monate" )

                Years ->
                    ( "Jahr", "Jahre" )
    in
    String.fromInt i
        ++ " "
        ++ (if i == 1 then
                singular

            else
                plural
           )
