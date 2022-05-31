module Graph.View.TestLabel exposing (..)

import Expect
import Test exposing (..)
import View.Graph.Label as Label


data : List ( String, List String )
data =
    [ ( "a label", [ "a label" ] )
    , ( "a label long", [ "a label", "long" ] )
    , ( "inter archive", [ "inter", "archive" ] )
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
