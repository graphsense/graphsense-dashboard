module Util.TextDimensions exposing (estimateTextWidth, estimateTextWidthAt, truncateToWidth)

import Dict exposing (Dict)


{-| Font size (px) at which the character dimensions in config are measured
(see `measureCharacterDimensions` in src/main.js).
-}
referenceFontSize : Float
referenceFontSize =
    12


{-| Estimate the width of text using character dimensions from config with fallback values.

Widths are reported for `referenceFontSize` px. Use `estimateTextWidthAt` to account
for a different rendered font size and letter spacing.

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


{-| Estimate the rendered width (px) of text drawn at a given font size and letter spacing.

The character dimensions are measured at `referenceFontSize`, so the base widths are
scaled by `fontSize / referenceFontSize`. Letter spacing is added once per character.

-}
estimateTextWidthAt : Dict String { width : Float, height : Float } -> { fontSize : Float, letterSpacing : Float } -> String -> Float
estimateTextWidthAt characterDimensions { fontSize, letterSpacing } text =
    estimateTextWidth characterDimensions text
        * (fontSize / referenceFontSize)
        + letterSpacing
        * toFloat (String.length text)


{-| Truncate `text` with a trailing ellipsis so that its estimated rendered width does
not exceed `maxWidth` px when drawn at the given font size and letter spacing.

Unlike a fixed character-count truncation, this accounts for per-character widths, so
strings full of wide glyphs (e.g. uppercase, "W", "M") are cut earlier than narrow ones.

-}
truncateToWidth : Dict String { width : Float, height : Float } -> { fontSize : Float, letterSpacing : Float, maxWidth : Float } -> String -> String
truncateToWidth characterDimensions { fontSize, letterSpacing, maxWidth } text =
    if estimateTextWidthAt characterDimensions { fontSize = fontSize, letterSpacing = letterSpacing } text <= maxWidth then
        text

    else
        let
            scale =
                fontSize / referenceFontSize

            ellipsisWidth =
                estimateTextWidthAt characterDimensions { fontSize = fontSize, letterSpacing = letterSpacing } "…"

            budget =
                maxWidth - ellipsisWidth

            takeChars chars acc accWidth =
                case chars of
                    [] ->
                        List.reverse acc

                    c :: rest ->
                        let
                            w =
                                getCharWidth characterDimensions c * scale + letterSpacing
                        in
                        if accWidth + w > budget then
                            List.reverse acc

                        else
                            takeChars rest (c :: acc) (accWidth + w)
        in
        (takeChars (String.toList text) [] 0 |> String.fromList) ++ "…"


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

        '&' ->
            8.0

        '%' ->
            10.0

        '@' ->
            11.4

        '#' ->
            6.9

        '*' ->
            4.8

        '_' ->
            5.5

        '"' ->
            4.3

        '\'' ->
            2.3

        '`' ->
            4.0

        '~' ->
            6.9

        '^' ->
            5.6

        '<' ->
            6.6

        '>' ->
            6.6

        '°' ->
            4.5

        '®' ->
            9.3

        '©' ->
            9.3

        '™' ->
            9.0

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
