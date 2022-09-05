module Tests exposing (suite)

import Test exposing (Test, describe)
import Tests.Parser.Expression
import Tests.Parser.Extras


suite : Test
suite =
    describe "Test Suite"
        [ Tests.Parser.Extras.suite
        , Tests.Parser.Expression.suite
        ]
