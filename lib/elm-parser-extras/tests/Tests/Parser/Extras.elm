module Tests.Parser.Extras exposing (suite)

import Expect
import Parser exposing ((|.), (|=), Parser)
import Parser.Extras
import Test exposing (Test, describe, test, todo)


type alias Point =
    { x : Float
    , y : Float
    }


simpleTerm : Parser Int
simpleTerm =
    Parser.int


complexTerm : Parser Point
complexTerm =
    Parser.succeed Point
        |. Parser.symbol "("
        |. Parser.spaces
        |= Parser.float
        |. Parser.spaces
        |. Parser.symbol ","
        |. Parser.spaces
        |= Parser.float
        |. Parser.spaces
        |. Parser.symbol ")"


suite : Test
suite =
    describe "Parser.Extras"
        [ many
        , some
        , escapedString
        ]


many : Test
many =
    describe "Parser.Extras.many"
        [ test "return an empty list when given an empty string (simpleTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.many simpleTerm) ""
                    |> Expect.equal (Ok [])
        , test "return an empty list when given an empty string (complexTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.many complexTerm) ""
                    |> Expect.equal (Ok [])
        , test "return the list of results (simpleTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.many simpleTerm) "42 1337"
                    |> Expect.equal (Ok [ 42, 1337 ])
        , test "return the list of results (complexTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.many complexTerm) "(42, 1337) (5, 25)"
                    |> Expect.equal (Ok [ Point 42 1337, Point 5 25 ])
        ]


some : Test
some =
    describe "Parser.Extras.some"
        [ test "errors when given an empty string (simpleTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.some simpleTerm) ""
                    |> Expect.err
        , test "errors when given an empty string (complexTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.some complexTerm) ""
                    |> Expect.err
        , test "returns a single element when given just one (simpleTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.some simpleTerm) "42"
                    |> Expect.equal (Ok ( 42, [] ))
        , test "returns a single element when given just one (complexTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.some complexTerm) "(42, 1337)"
                    |> Expect.equal (Ok ( Point 42 1337, [] ))
        , test "returns the expected elements when given more than one (simpleTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.some simpleTerm) "42 1337"
                    |> Expect.equal (Ok ( 42, [ 1337 ] ))
        , test "returns the expected elements when given more than one (complexTerm)" <|
            \_ ->
                Parser.run (Parser.Extras.some complexTerm) "(42, 1337) (5, 25)"
                    |> Expect.equal (Ok ( Point 42 1337, [ Point 5 25 ] ))
        ]


escapedString : Test
escapedString =
    describe "Parser.Extras.escapedString"
        [ test "parse string with escaped characters" <|
            \_ ->
                Parser.run (Parser.Extras.quotedString '\\' '"') "\"str\\\"\""
                    |> Expect.equal (Ok "str\"")
        , test "parse string with escaped character in beginning" <|
            \_ ->
                Parser.run (Parser.Extras.quotedString '\\' '"') "\"\\\"str\""
                    |> Expect.equal (Ok "\"str")
        , test "parse string with escaped character in middle" <|
            \_ ->
                Parser.run (Parser.Extras.quotedString '\\' '"') "\"xyz\\\"str\""
                    |> Expect.equal (Ok "xyz\"str")
        , test "parse string with double escaped characters" <|
            \_ ->
                Parser.run (Parser.Extras.quotedString '\\' '"') "\"str\\\"\\\"\""
                    |> Expect.equal (Ok "str\"\"")
        , test "parse string with other escaped characters" <|
            \_ ->
                Parser.run (Parser.Extras.quotedString '\\' '"') "\"str\\x\""
                    |> Expect.equal (Ok "str\\x")
        , test "parse string with equal quote and escape char" <|
            \_ ->
                Parser.run (Parser.Extras.quotedString '\'' '\'') "'str'''"
                    |> Expect.equal (Ok "str'")
        ]
