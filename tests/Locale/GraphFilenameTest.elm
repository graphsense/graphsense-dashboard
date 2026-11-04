module Locale.GraphFilenameTest exposing (suite)

{-| Downloaded graph files (.gs, PDF, PNG) are named after the timestamp and
the graph name, e.g. the record a plugin opened the graph for.
-}

import Config.UserSettings
import Expect
import Init.Locale
import Model.Locale
import Test exposing (Test, describe, test)
import Time
import View.Locale exposing (makeGraphFilename, makeTimestampFilename)


locale : Model.Locale.Model
locale =
    Config.UserSettings.default "en"
        |> Init.Locale.init
        |> Tuple.first


t : Time.Posix
t =
    Time.millisToPosix 1793800000000


stamp : String
stamp =
    makeTimestampFilename locale t


suite : Test
suite =
    describe "View.Locale.makeGraphFilename"
        [ test "adds the graph name after the timestamp" <|
            \_ ->
                makeGraphFilename locale t "graph"
                    |> Expect.equal (stamp ++ " graph")
        , test "a name with a slash, as in a record reference, stays one file name" <|
            \_ ->
                makeGraphFilename locale t "AB-123 / 2026"
                    |> Expect.equal (stamp ++ " AB-123 - 2026")
        , test "characters not allowed in file names become dashes" <|
            \_ ->
                makeGraphFilename locale t "a:b*c?d\"e<f>g|h\\i"
                    |> Expect.equal (stamp ++ " a-b-c-d-e-f-g-h-i")
        , test "blank names leave the timestamp alone" <|
            \_ ->
                makeGraphFilename locale t "  \t "
                    |> Expect.equal stamp
        , test "runs of whitespace collapse to one space" <|
            \_ ->
                makeGraphFilename locale t "  my   case \n x "
                    |> Expect.equal (stamp ++ " my case x")
        ]
