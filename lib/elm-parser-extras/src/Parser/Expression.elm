module Parser.Expression exposing
    ( Operator(..), OperatorTable, Assoc(..)
    , prefixOperator, infixOperator, postfixOperator
    , buildExpressionParser
    )

{-| Tools for building parsers for prefix, postfix or infix operator expressions.


# Types

@docs Operator, OperatorTable, Assoc


# Operators

@docs prefixOperator, infixOperator, postfixOperator


# Builder

@docs buildExpressionParser

-}

import Parser exposing (..)
import Tuple


{-| The representation for an operator parser.
An operator can either be binary infix and require an associativity,
or it can be unary prefix or postfix.
-}
type Operator a
    = Infix (Parser (a -> a -> a)) Assoc
    | Prefix (Parser (a -> a))
    | Postfix (Parser (a -> a))


{-| This is just a List of Lists of Operators.
The first inner list has the highest precedence and the last has the lowest.
If two operators are on the same inner list, they have the same precedence.
-}
type alias OperatorTable a =
    List (List (Operator a))


{-| The associativity of an operator.
-}
type Assoc
    = AssocNone
    | AssocLeft
    | AssocRight


{-| Create a prefix operator parser from a unary function
and a parser for the operator symbol.
-}
prefixOperator : (a -> a) -> Parser () -> Operator a
prefixOperator fn opParser =
    Prefix (unaryOp fn opParser)


{-| Create an infix operator parser from a binary function,
a parser for the operator symbol and an associativity.
-}
infixOperator : (a -> a -> a) -> Parser () -> Assoc -> Operator a
infixOperator fn opParser assoc =
    Infix (binaryOp fn opParser) assoc


{-| Create a postfix operator parser from a unary function
and a parser for the operator symbol.
-}
postfixOperator : (a -> a) -> Parser () -> Operator a
postfixOperator fn opParser =
    Postfix (unaryOp fn opParser)


{-| Build an expression parser for terms from a table of operators,
taking into account precedence and associativity.

The following would define a simple arithmetic parser.

    operators : OperatorTable number
    operators =
        [ [ prefixOperator negate (symbol "-"), prefixOperator identity (symbol "+") ]
        , [ postfixOperator (\x -> x + 1) (symbol "++") ]
        , [ infixOperator (*) (symbol "*") AssocLeft ]
        , [ infixOperator (+) (symbol "+") AssocLeft, infixOperator (-) (symbol "-") AssocLeft ]
        ]

    term : Parser Int
    term =
        oneOf
            [ parens (lazy (\_ -> expr))
                |. spaces
            , int
                |. spaces
            ]

    expr : Parser Int
    expr =
        buildExpressionParser operators (lazy <| \_ -> term)

-}
buildExpressionParser : OperatorTable a -> Parser a -> Parser a
buildExpressionParser operators simpleExpr =
    List.foldl makeParser simpleExpr operators



-- HELPERS


makeParser : List (Operator a) -> Parser a -> Parser a
makeParser ops term =
    let
        { rassoc, lassoc, nassoc, prefix, postfix } =
            List.foldr splitOp initOps ops

        rassocOp : Parser (a -> a -> a)
        rassocOp =
            oneOf rassoc

        lassocOp : Parser (a -> a -> a)
        lassocOp =
            oneOf lassoc

        nassocOp : Parser (a -> a -> a)
        nassocOp =
            oneOf nassoc

        prefixOp : Parser (a -> a)
        prefixOp =
            oneOf prefix

        postfixOp : Parser (a -> a)
        postfixOp =
            oneOf postfix

        ambiguous : String -> Parser (a -> a -> a) -> Parser a
        ambiguous assoc op =
            backtrackable
                (op
                    |> andThen (\_ -> problem ("ambiguous use of a " ++ assoc ++ " associative operator"))
                )

        ambiguousRight : Parser a
        ambiguousRight =
            ambiguous "right" rassocOp

        ambiguousLeft : Parser a
        ambiguousLeft =
            ambiguous "left" lassocOp

        ambiguousNon : Parser a
        ambiguousNon =
            ambiguous "non" nassocOp

        termP : Parser a
        termP =
            succeed (\pre x post -> post (pre x))
                |= prefixP
                |= term
                |= postfixP

        prefixP : Parser (a -> a)
        prefixP =
            oneOf
                [ prefixOp
                , succeed identity
                ]

        postfixP : Parser (a -> a)
        postfixP =
            oneOf
                [ postfixOp
                , succeed identity
                ]

        rassocP : a -> Parser a
        rassocP x =
            oneOf
                [ succeed (\f y -> f x y)
                    |= rassocOp
                    |= (termP |> andThen rassocP1)
                , ambiguousLeft
                , ambiguousNon
                ]

        rassocP1 : a -> Parser a
        rassocP1 x =
            oneOf
                [ rassocP x
                , succeed x
                ]

        lassocP : a -> Parser a
        lassocP x =
            oneOf
                [ succeed Tuple.pair
                    |= lassocOp
                    |= termP
                    |> andThen (\( f, y ) -> lassocP1 (f x y))
                , ambiguousRight
                , ambiguousNon
                ]

        lassocP1 : a -> Parser a
        lassocP1 x =
            oneOf
                [ lassocP x
                , succeed x
                ]

        nassocP : a -> Parser a
        nassocP x =
            succeed Tuple.pair
                |= nassocOp
                |= termP
                |> andThen (\( f, y ) -> oneOf [ ambiguousRight, ambiguousLeft, ambiguousNon, succeed (f x y) ])
    in
    termP
        |> andThen (\x -> oneOf [ rassocP x, lassocP x, nassocP x, succeed x ])


type alias Ops a =
    { rassoc : List (Parser (a -> a -> a))
    , lassoc : List (Parser (a -> a -> a))
    , nassoc : List (Parser (a -> a -> a))
    , prefix : List (Parser (a -> a))
    , postfix : List (Parser (a -> a))
    }


initOps =
    { rassoc = [], lassoc = [], nassoc = [], prefix = [], postfix = [] }


splitOp : Operator a -> Ops a -> Ops a
splitOp operator ({ rassoc, lassoc, nassoc, prefix, postfix } as ops) =
    case operator of
        Infix op AssocNone ->
            { ops | nassoc = op :: ops.nassoc }

        Infix op AssocLeft ->
            { ops | lassoc = op :: ops.lassoc }

        Infix op AssocRight ->
            { ops | rassoc = op :: ops.rassoc }

        Prefix op ->
            { ops | prefix = op :: ops.prefix }

        Postfix op ->
            { ops | postfix = op :: ops.postfix }


unaryOp : (a -> a) -> Parser () -> Parser (a -> a)
unaryOp fn opParser =
    succeed fn
        |. opParser
        |. spaces


binaryOp : (a -> a -> a) -> Parser () -> Parser (a -> a -> a)
binaryOp fn opParser =
    succeed fn
        |. opParser
        |. spaces
