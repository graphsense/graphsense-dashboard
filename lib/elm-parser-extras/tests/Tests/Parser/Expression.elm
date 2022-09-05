module Tests.Parser.Expression exposing (suite)

import Expect
import Parser exposing ((|.), (|=), Parser)
import Parser.Expression exposing (Assoc(..), OperatorTable)
import Parser.Extras
import Test exposing (Test, describe, test, todo)


operators : OperatorTable number
operators =
    [ [ Parser.Expression.prefixOperator
            negate
            (Parser.symbol "-")
      , Parser.Expression.prefixOperator
            identity
            (Parser.symbol "+")
      ]
    , [ Parser.Expression.postfixOperator
            (\x -> x + 1)
            (Parser.symbol "++")
      ]
    , [ Parser.Expression.infixOperator
            (*)
            (Parser.symbol "*")
            AssocLeft
      ]
    , [ Parser.Expression.infixOperator
            (+)
            (Parser.symbol "+")
            AssocLeft
      , Parser.Expression.infixOperator
            (-)
            (Parser.symbol "-")
            AssocLeft
      ]
    ]


term : Parser Int
term =
    Parser.oneOf
        [ Parser.Extras.parens (Parser.lazy (\_ -> expr))
            |. Parser.spaces
        , Parser.int
            |. Parser.spaces
        ]


expr : Parser Int
expr =
    Parser.Expression.buildExpressionParser operators (Parser.lazy <| \_ -> term)


suite : Test
suite =
    describe "Parser.Expression"
        [ expression ]


expression : Test
expression =
    describe "Example case"
        [ describe "Infix"
            [ test "Simple expression: 1 + 1" <|
                \_ ->
                    Parser.run expr "1 + 1"
                        |> Expect.equal (Ok 2)
            , test "Simple expression: 1 -1 " <|
                \_ ->
                    Parser.run expr "1 - 1"
                        |> Expect.equal (Ok 0)
            , test "Precedence: 1 + 1 * 10 - 1" <|
                \_ ->
                    Parser.run expr "1 + 1 * 10 - 1"
                        |> Expect.equal (Ok 10)
            , test "Grouping: (1 + 1) * 10" <|
                \_ ->
                    Parser.run expr "(1 + 1) * 10 - 1"
                        |> Expect.equal (Ok 19)
            ]
        , describe "Prefix"
            [ test "Positive" <|
                \_ ->
                    Parser.run expr "+25"
                        |> Expect.equal (Ok 25)
            , test "Negative" <|
                \_ ->
                    Parser.run expr "-25"
                        |> Expect.equal (Ok -25)
            , test "Double negative" <|
                \_ ->
                    Parser.run expr "-(-25)"
                        |> Expect.equal (Ok 25)
            ]
        , describe "Postfix"
            [ test "Increment" <|
                \_ ->
                    Parser.run expr "41++"
                        |> Expect.equal (Ok 42)
            , test "Increment negative (prefix has higher precedence than postfix)" <|
                \_ ->
                    Parser.run expr "-43++"
                        |> Expect.equal (Ok -42)
            ]
        , describe "Mix and match"
            [ test "Complex expression 1" <|
                \_ ->
                    Parser.run expr "((41++ - 42) + 1) * 100"
                        |> Expect.equal (Ok 100)
            , test "Complex expression 2" <|
                \_ ->
                    Parser.run expr "-((41++ - 42) + 1) * 100"
                        |> Expect.equal (Ok -100)
            , test "Complex expression 3" <|
                \_ ->
                    Parser.run expr "1--1"
                        |> Expect.equal (Ok 2)
            ]
        ]
