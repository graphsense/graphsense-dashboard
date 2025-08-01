module Util.TextDimensions exposing (estimateTextWidth)

import Dict exposing (Dict)


{-| Estimate the width of text using character dimensions from config with fallback values
-}
estimateTextWidth : Dict String { width : Float, height : Float } -> String -> Float
estimateTextWidth characterDimensions text =
    if String.isEmpty text then
        0

    else
        text
            |> String.toList
            |> List.map (getCharWidth characterDimensions)
            |> List.sum


{-| Get character width from config with fallback to hardcoded values
-}
getCharWidth : Dict String { width : Float, height : Float } -> Char -> Float
getCharWidth dimensions char =
    let
        charString =
            String.fromChar char
    in
    case Dict.get charString dimensions of
        Just dimension ->
            dimension.width

        Nothing ->
            -- Fallback to hardcoded values if character not found in config
            getCharWidthFallback char


{-| Fallback character width mapping based on actual browser measurements

Values based on Roboto 12px font size

-}
getCharWidthFallback : Char -> Float
getCharWidthFallback char =
    case char of
        -- Numbers
        '0' ->
            6.7

        '1' ->
            6.7

        '2' ->
            6.7

        '3' ->
            6.7

        '4' ->
            6.7

        '5' ->
            6.7

        '6' ->
            6.7

        '7' ->
            6.7

        '8' ->
            6.7

        '9' ->
            6.7

        -- Uppercase letters
        'A' ->
            7.8

        'B' ->
            7.5

        'C' ->
            7.8

        'D' ->
            7.9

        'E' ->
            6.8

        'F' ->
            6.6

        'G' ->
            8.2

        'H' ->
            8.6

        'I' ->
            3.3

        'J' ->
            6.6

        'K' ->
            7.5

        'L' ->
            6.5

        'M' ->
            10.5

        'N' ->
            8.6

        'O' ->
            8.3

        'P' ->
            7.6

        'Q' ->
            8.3

        'R' ->
            7.4

        'S' ->
            7.1

        'T' ->
            7.2

        'U' ->
            7.8

        'V' ->
            7.6

        'W' ->
            10.6

        'X' ->
            7.5

        'Y' ->
            7.2

        'Z' ->
            7.2

        -- Lowercase letters
        'a' ->
            6.5

        'b' ->
            6.7

        'c' ->
            6.3

        'd' ->
            6.8

        'e' ->
            6.4

        'f' ->
            4.2

        'g' ->
            6.7

        'h' ->
            6.6

        'i' ->
            2.9

        'j' ->
            2.9

        'k' ->
            6.1

        'l' ->
            2.9

        'm' ->
            10.5

        'n' ->
            6.6

        'o' ->
            6.8

        'p' ->
            6.7

        'q' ->
            6.8

        'r' ->
            4.1

        's' ->
            6.2

        't' ->
            3.9

        'u' ->
            6.6

        'v' ->
            5.8

        'w' ->
            9.0

        'x' ->
            5.9

        'y' ->
            5.7

        'z' ->
            5.9

        -- Special characters and symbols
        ' ' ->
            3.0

        '.' ->
            3.2

        ',' ->
            2.4

        ':' ->
            2.9

        ';' ->
            2.5

        '!' ->
            3.1

        '?' ->
            5.7

        '-' ->
            3.3

        '+' ->
            6.8

        '=' ->
            6.6

        '(' ->
            4.1

        ')' ->
            4.2

        '[' ->
            3.2

        ']' ->
            3.2

        '{' ->
            4.1

        '}' ->
            4.1

        '|' ->
            2.9

        '/' ->
            4.9

        '\\' ->
            4.9

        -- Currency symbols
        '$' ->
            6.7

        '€' ->
            6.7

        '£' ->
            7.0

        '¥' ->
            6.3

        '¢' ->
            6.6

        -- Default for unmeasured characters
        _ ->
            6.0
