module InternalTest exposing (suite)

import Expect
import Internal exposing (KeyDown(..), calculateIndex)
import Test exposing (..)


suite : Test
suite =
    describe "Internal Test"
        [ calculateIndexTest
        ]


calculateIndexTest : Test
calculateIndexTest =
    let
        listOfThree : Maybe Int -> KeyDown -> Maybe Int
        listOfThree =
            calculateIndex 3
    in
    describe "calculateIndex"
        [ test "ArrowUp: Selects last index when Nothing" <|
            \_ ->
                Expect.equal (listOfThree Nothing ArrowUp) (Just 2)
        , test "ArrowUp: Selects previous index" <|
            \_ ->
                Expect.equal (listOfThree (Just 1) ArrowUp) (Just 0)
        , test "ArrowUp: Wraps to bottom" <|
            \_ ->
                Expect.equal (listOfThree (Just 0) ArrowUp) (Just 2)
        , test "ArrowDown: Selects first index when Nothing" <|
            \_ ->
                Expect.equal (listOfThree Nothing ArrowDown) (Just 0)
        , test "ArrowDown: Selects next index" <|
            \_ ->
                Expect.equal (listOfThree (Just 1) ArrowDown) (Just 2)
        , test "ArrowDown: Wraps to top" <|
            \_ ->
                Expect.equal (listOfThree (Just 2) ArrowDown) (Just 0)
        ]
