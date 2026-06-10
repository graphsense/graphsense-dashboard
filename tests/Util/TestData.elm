module Util.TestData exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util.Data exposing (normalizeIdCasing)


suite : Test
suite =
    describe "normalizeIdCasing"
        [ test "lowercases eth addresses" <|
            \_ ->
                normalizeIdCasing "eth" "0xFB50526f49894B78541b776F5aaefe43e3bd8590"
                    |> Expect.equal "0xfb50526f49894b78541b776f5aaefe43e3bd8590"
        , test "lowercases eth tx hashes" <|
            \_ ->
                normalizeIdCasing "eth" "0xA9059CBB2AB09EB219583F4A59A5D0623ADE346D962BCD4E46B11DA047C9049B"
                    |> Expect.equal "0xa9059cbb2ab09eb219583f4a59a5d0623ade346d962bcd4e46b11da047c9049b"
        , test "preserves sub-tx markers of token txs" <|
            \_ ->
                normalizeIdCasing "eth" "0xA9059CBB2AB09EB219583F4A59A5D0623ADE346D962BCD4E46B11DA047C9049B_T1"
                    |> Expect.equal "0xa9059cbb2ab09eb219583f4a59a5d0623ade346d962bcd4e46b11da047c9049b_T1"
        , test "preserves sub-tx markers of internal txs" <|
            \_ ->
                normalizeIdCasing "eth" "0xA9059CBB2AB09EB219583F4A59A5D0623ADE346D962BCD4E46B11DA047C9049B_I12"
                    |> Expect.equal "0xa9059cbb2ab09eb219583f4a59a5d0623ade346d962bcd4e46b11da047c9049b_I12"
        , test "is case-insensitive on the network" <|
            \_ ->
                normalizeIdCasing "ETH" "0xFB50526f49894B78541b776F5aaefe43e3bd8590"
                    |> Expect.equal "0xfb50526f49894b78541b776f5aaefe43e3bd8590"
        , test "leaves btc addresses untouched" <|
            \_ ->
                normalizeIdCasing "btc" "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2"
                    |> Expect.equal "1BvBMSEYstWetqTFn5Au4m4GFg7xJaNVN2"
        , test "leaves trx addresses untouched" <|
            \_ ->
                normalizeIdCasing "trx" "TJRabPrwbZy45sbavfcjinPJC18kjpRTv8"
                    |> Expect.equal "TJRabPrwbZy45sbavfcjinPJC18kjpRTv8"
        ]
