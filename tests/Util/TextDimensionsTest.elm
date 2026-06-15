module Util.TextDimensionsTest exposing (suite)

import Dict exposing (Dict)
import Expect
import Test exposing (Test, describe, test)
import Util.TextDimensions exposing (estimateTextWidth, estimateTextWidthAt, truncateToWidth)


{-| All tests use an empty dimensions dict, so they exercise the deterministic
hardcoded fallback widths (Roboto 12px) rather than runtime measurements.
-}
dims : Dict String { width : Float, height : Float }
dims =
    Dict.empty


suite : Test
suite =
    describe "Util.TextDimensions"
        [ describe "estimateTextWidth"
            [ test "empty string has zero width" <|
                \_ ->
                    estimateTextWidth dims ""
                        |> Expect.within (Expect.Absolute 0.001) 0
            , test "sums per-character fallback widths" <|
                \_ ->
                    -- 'W' = 10.6, 'i' = 2.9
                    estimateTextWidth dims "Wi"
                        |> Expect.within (Expect.Absolute 0.001) 13.5
            , test "handles special characters from the fallback table" <|
                \_ ->
                    -- '$' = 6.7, '&' = 8.0, '@' = 11.4
                    estimateTextWidth dims "$&@"
                        |> Expect.within (Expect.Absolute 0.001) 26.1
            ]
        , describe "estimateTextWidthAt"
            [ test "scales width with font size relative to the 12px reference" <|
                \_ ->
                    -- 'W' = 10.6 at 12px -> doubled at 24px
                    estimateTextWidthAt dims { fontSize = 24, letterSpacing = 0 } "W"
                        |> Expect.within (Expect.Absolute 0.001) 21.2
            , test "adds letter spacing once per character" <|
                \_ ->
                    -- "ii" = 2*2.9 = 5.8, plus 0.4 letter spacing per char = +0.8
                    estimateTextWidthAt dims { fontSize = 12, letterSpacing = 0.4 } "ii"
                        |> Expect.within (Expect.Absolute 0.001) 6.6
            ]
        , describe "truncateToWidth"
            [ test "leaves text that fits untouched" <|
                \_ ->
                    truncateToWidth dims { fontSize = 12, letterSpacing = 0, maxWidth = 20 } "iiiii"
                        |> Expect.equal "iiiii"
            , test "cuts wide-glyph text earlier than narrow text at the same budget" <|
                \_ ->
                    -- Budget 20px, ellipsis '…' = 6.0 -> 14px for content.
                    -- 'W' = 10.6, so only one fits before the ellipsis.
                    truncateToWidth dims { fontSize = 12, letterSpacing = 0, maxWidth = 20 } "WWWWW"
                        |> Expect.equal "W…"
            , test "appends an ellipsis when truncating" <|
                \_ ->
                    truncateToWidth dims { fontSize = 12, letterSpacing = 0, maxWidth = 20 } "WWWWW"
                        |> String.endsWith "…"
                        |> Expect.equal True
            ]
        ]
