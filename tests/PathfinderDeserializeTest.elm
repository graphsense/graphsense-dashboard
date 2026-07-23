module PathfinderDeserializeTest exposing (suite)

import Expect
import Json.Encode
import Test exposing (Test, test)
import Update.Pathfinder


suite : Test
suite =
    -- plugins (e.g. Casemgm) send this envelope with empty lists to reset the
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
