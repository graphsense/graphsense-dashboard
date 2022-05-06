module Graph.View.TestLabel exposing (..)

import Expect
import Graph.View.Label as Label
import Test exposing (..)


data : List ( String, List String )
data =
    [ ( "a label", [ "a label" ] )
    , ( "a label long", [ "a label", "long" ] )
    , ( "alabellong", [ "alabello", "ng" ] )
    , ( "alabellong space", [ "alabello", "ng space" ] )
    , ( "a label that is way too long", [ "a label", "that is", "way too", "long" ] )
    , ( "a lab thatiswaytoolong", [ "a lab th", "atiswayt", "oolong" ] )
    , ( "alabelthatiswaytoolong", [ "alabelth", "atiswayt", "oolong" ] )
    , ( "", [ "" ] )
    ]


suite : Test
suite =
    describe "The Graph.View.Label module"
        (data
            |> List.map
                (\( input, output ) ->
                    test ("input: \"" ++ input ++ "\"") <|
                        \_ ->
                            Expect.equal output (Label.split 8 input)
                )
        )
