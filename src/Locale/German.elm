module Locale.German exposing (german)

import DateFormat.Language exposing (Language)
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
