module PathfinderDeserializeTest exposing (suite)

import Expect
import Json.Encode
import Test exposing (Test, describe, test)
import Update.Pathfinder


suite : Test
suite =
    describe "deserialize"
        [ -- plugins (e.g. Casemgm) send this envelope with empty lists to reset the
          -- pathfinder graph when opening a case that has no stored graph
          test "deserialize accepts an empty pathfinder graph envelope" <|
            \_ ->
                [ Json.Encode.string "pathfinder"
                , Json.Encode.string "1"
                , Json.Encode.string "empty"
                , Json.Encode.list identity []
                , Json.Encode.list identity []
                , Json.Encode.list identity []
                , Json.Encode.list identity []
                ]
                    |> Json.Encode.list identity
                    |> Update.Pathfinder.deserialize
                    |> Result.map
                        (\d ->
                            List.isEmpty d.addresses
                                && List.isEmpty d.txs
                                && List.isEmpty d.annotations
                                && List.isEmpty d.aggEdges
                        )
                    |> Expect.equal (Ok True)
        , describe "isLegacyPf1GsFile"
            ([ ( "0.4.4", True )
             , ( "0.4.5", True )
             , ( "0.5.1", True )
             , ( "1.0.0", True )

             -- main.js strips " "/"-" suffixes, but be liberal about raw tags
             , ( "0.5.0-dev", True )
             , ( "pathfinder", False )
             ]
                |> List.map
                    (\( version, expected ) ->
                        test ("first element \"" ++ version ++ "\"") <|
                            \_ ->
                                [ Json.Encode.string version ]
                                    |> Json.Encode.list identity
                                    |> Update.Pathfinder.isLegacyPf1GsFile
                                    |> Expect.equal expected
                    )
            )
        , test "isLegacyPf1GsFile rejects non-array payloads" <|
            \_ ->
                Json.Encode.object [ ( "version", Json.Encode.string "0.5.1" ) ]
                    |> Update.Pathfinder.isLegacyPf1GsFile
                    |> Expect.equal False
        ]
