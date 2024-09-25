module Graph.View.TestLabel exposing (suite)

import Expect
import Test
import View.Graph.Label as Label


data : List ( String, List String )
data =
    [ ( "a label", [ "a label" ] )
    , ( "a label long", [ "a label", "long" ] )
    , ( "inter archive", [ "inter ar", "chive" ] )
    , ( "alabellong", [ "alabello", "ng" ] )
    , ( "alabellong space", [ "alabello", "ng space" ] )
    , ( "a label that is way too long", [ "a label", "that is", "way too", "long" ] )
    , ( "a lab thatiswaytoolong", [ "a lab th", "atiswayt", "oolong" ] )
    , ( "alabelthatiswaytoolong", [ "alabelth", "atiswayt", "oolong" ] )
    , ( "", [ "" ] )
    ]


suite : Test.Test
suite =
    Test.describe "The Graph.View.Label module"
        (data
            |> List.map
                (\( input, output ) ->
                    Test.test ("input: \"" ++ input ++ "\"") <|
                        \_ ->
                            Expect.equal output (Label.split 8 input)
                )
        )
