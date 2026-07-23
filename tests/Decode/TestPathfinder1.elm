module Decode.TestPathfinder1 exposing (suite)

import Decode.Pathfinder1
import Expect
import Json.Decode
import Test exposing (Test, describe, test)


gsFile : String
gsFile =
    """
    [ "pathfinder"
    , "1"
    , "my graph"
    , [ [ [ "eth", "0xFB50526f49894B78541b776F5aaefe43e3bd8590" ], 1, 2, true, 0 ]
      , [ [ "btc", "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2" ], 3, 4, false, 0 ]
      ]
    , [ [ [ "eth", "0xA9059CBB2AB09EB219583F4A59A5D0623ADE346D962BCD4E46B11DA047C9049B_T1" ], 5, 6, false, 0 ]
      ]
    , [ [ [ "ETH", "0xFB50526f49894B78541b776F5aaefe43e3bd8590" ], "a label" ]
      ]
    ]
    """


suite : Test
suite =
    describe "Decode.Pathfinder1"
        [ test "normalizes id casing of eth addresses and txs" <|
            \_ ->
                case Json.Decode.decodeString Decode.Pathfinder1.decoder gsFile of
                    Ok deserialized ->
                        { addressIds = deserialized.addresses |> List.map .id
                        , txIds = deserialized.txs |> List.map .id
                        , annotationIds = deserialized.annotations |> List.map .id
                        }
                            |> Expect.equal
                                { addressIds =
                                    [ ( "eth", "0xfb50526f49894b78541b776f5aaefe43e3bd8590" )
                                    , ( "btc", "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2" )
                                    ]
                                , txIds =
                                    [ ( "eth", "0xa9059cbb2ab09eb219583f4a59a5d0623ade346d962bcd4e46b11da047c9049b_T1" )
                                    ]
                                , annotationIds =
                                    [ ( "eth", "0xfb50526f49894b78541b776f5aaefe43e3bd8590" )
                                    ]
                                }

                    Err err ->
                        Expect.fail (Json.Decode.errorToString err)
        ]
