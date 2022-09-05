module Parser.Extras exposing
    ( many, some, between, parens, braces, brackets
    , quotedString
    )

{-| Some convenience parser combinators.


# Combinators

@docs many, some, between, parens, braces, brackets

-}

import Parser exposing (..)
import Tuple


{-| Apply a parser zero or more times and return a list of the results.
-}
many : Parser a -> Parser (List a)
many p =
    loop [] (manyHelp p)


{-| Apply a parser one or more times and return a tuple of the first result parsed
and the list of the remaining results.
-}
some : Parser a -> Parser ( a, List a )
some p =
    succeed Tuple.pair
        |= p
        |. spaces
        |= many p


{-| Parse an expression between two other parsers
-}
between : Parser opening -> Parser closing -> Parser a -> Parser a
between opening closing p =
    succeed identity
        |. opening
        |. spaces
        |= p
        |. spaces
        |. closing


{-| Parse an expression between parenthesis.

    parens p == between (symbol "(") (symbol ")") p

-}
parens : Parser a -> Parser a
parens =
    between (symbol "(") (symbol ")")


{-| Parse an expression between curly braces.

    braces p == between (symbol "{") (symbol "}") p

-}
braces : Parser a -> Parser a
braces =
    between (symbol "{") (symbol "}")


{-| Parse an expression between square brackets.

    brackets p == between (symbol "[") (symbol "]") p

-}
brackets : Parser a -> Parser a
brackets =
    between (symbol "[") (symbol "]")


{-| Parse a quoted string with an escape character
-}
quotedString : Char -> Char -> Parser String
quotedString escape quote =
    succeed identity
        |. chompIf ((==) quote)
        |= loop "" (quotedStringHelp escape quote)



-- HELPERS


manyHelp : Parser a -> List a -> Parser (Step (List a) (List a))
manyHelp p vs =
    oneOf
        [ succeed (\v -> Loop (v :: vs))
            |= p
            |. spaces
        , succeed ()
            |> map (\_ -> Done (List.reverse vs))
        ]


quotedStringHelp : Char -> Char -> String -> Parser (Step String String)
quotedStringHelp escape quote string =
    oneOf
        [ token (String.fromList [ escape, quote ])
            |> map (\_ -> string ++ String.fromChar quote |> Loop)
        , chompIf ((==) quote)
            |> map (\_ -> Done string)
        , chompIf ((==) escape)
            |> map (\_ -> string ++ String.fromChar escape |> Loop)
        , (getChompedString <|
            succeed ()
                |. chompWhile (\c -> c /= quote && c /= escape)
          )
            |> map (\s -> string ++ s |> Loop)
        ]
