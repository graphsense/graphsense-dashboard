module PathfinderDeserializeTest exposing (suite)

import Dict
import Expect
import Json.Encode
import Support.App as App
import Support.Env as Env
import Test exposing (Test, describe, test)
import Update.Pathfinder


suite : Test
suite =
    describe "deserialize"
        [ -- plugins send this envelope with empty lists to reset the
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
        , test "an uppercase bech32 id from a file becomes the lowercase form the API serves, everywhere it is used" <|
            \_ ->
                -- BIP173 allows all uppercase (QR codes); files saved from an
                -- uppercase deep link carry that spelling
                gsFile
                    [ thing "btc" "BC1QX8HYAG2NVJJKKHDGJRSLWLYKM86PFLQNGEXAXN" ]
                    [ annotation "btc" "BC1QX8HYAG2NVJJKKHDGJRSLWLYKM86PFLQNGEXAXN" "suspect" ]
                    |> Update.Pathfinder.deserialize
                    |> Result.map (\d -> ( List.map .id d.addresses, List.map .id d.annotations ))
                    |> Expect.equal
                        (Ok
                            ( [ ( "btc", "bc1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn" ) ]
                            , [ ( "btc", "bc1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn" ) ]
                            )
                        )
        , test "a file saved with both spellings of one bech32 address opens with one node" <|
            \_ ->
                gsFile
                    [ thing "btc" "BC1QX8HYAG2NVJJKKHDGJRSLWLYKM86PFLQNGEXAXN"
                    , thing "btc" "bc1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn"
                    ]
                    []
                    |> Update.Pathfinder.deserialize
                    |> Result.map
                        (\d ->
                            Update.Pathfinder.fromDeserialized Env.updateConfig d (App.model App.init)
                                |> Tuple.first
                                |> .network
                                |> .addresses
                                |> Dict.keys
                        )
                    |> Expect.equal (Ok [ ( "btc", "bc1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn" ) ])
        , test "a base58 id from a file that starts like bech32 keeps its case" <|
            \_ ->
                gsFile [ thing "ltc" "LTC1A4GhnLkKbpfkaDyXHTeShP2YksUVgY" ] []
                    |> Update.Pathfinder.deserialize
                    |> Result.map (\d -> List.map .id d.addresses)
                    |> Expect.equal (Ok [ ( "ltc", "LTC1A4GhnLkKbpfkaDyXHTeShP2YksUVgY" ) ])
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


{-| A saved graph with these addresses and annotations, nothing else.
-}
gsFile : List Json.Encode.Value -> List Json.Encode.Value -> Json.Encode.Value
gsFile addresses annotations =
    [ Json.Encode.string "pathfinder"
    , Json.Encode.string "1"
    , Json.Encode.string "test"
    , Json.Encode.list identity addresses
    , Json.Encode.list identity []
    , Json.Encode.list identity annotations
    , Json.Encode.list identity []
    ]
        |> Json.Encode.list identity


thing : String -> String -> Json.Encode.Value
thing network identifier =
    Json.Encode.list identity
        [ Json.Encode.list Json.Encode.string [ network, identifier ]
        , Json.Encode.float 1
        , Json.Encode.float 2
        , Json.Encode.bool False
        , Json.Encode.int 0
        ]


annotation : String -> String -> String -> Json.Encode.Value
annotation network identifier label =
    Json.Encode.list identity
        [ Json.Encode.list Json.Encode.string [ network, identifier ]
        , Json.Encode.string label
        ]
