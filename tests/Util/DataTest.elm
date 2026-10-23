module Util.DataTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util.Data as Data


suite : Test
suite =
    describe "Util.Data"
        [ looksLikeTxHashSuite
        , normalizeIdentifierSuite
        , isMixedCaseBech32Suite
        ]


looksLikeTxHashSuite : Test
looksLikeTxHashSuite =
    describe "Util.Data.looksLikeTxHash"
        [ test "64 hex digits are a transaction hash" <|
            \_ ->
                Data.looksLikeTxHash "D8FBA40D0E331BCE4EB2D9427635E76E12DC1D6DED26D77CDF8559BCDE07B715"
                    |> Expect.equal True
        , test "with a leading 0x as well" <|
            \_ ->
                Data.looksLikeTxHash "0xd8fba40d0e331bce4eb2d9427635e76e12dc1d6ded26d77cdf8559bcde07b715"
                    |> Expect.equal True
        , test "an address is not" <|
            \_ ->
                Data.looksLikeTxHash "1Archive1n2C579dMsAu3iC6tWzuQJz8dN"
                    |> Expect.equal False
        , test "64 characters that are not all hex are not either" <|
            \_ ->
                Data.looksLikeTxHash "G8FBA40D0E331BCE4EB2D9427635E76E12DC1D6DED26D77CDF8559BCDE07B715"
                    |> Expect.equal False
        ]


normalizeIdentifierSuite : Test
normalizeIdentifierSuite =
    -- BIP173 allows a bech32 address in all lowercase or all uppercase (QR
    -- codes use uppercase). The API serves the lowercase form, so graph node
    -- ids must use it too, or one address becomes two nodes.
    describe "Util.Data.normalizeIdentifier"
        [ test "an uppercase btc bech32 address becomes lowercase" <|
            \_ ->
                Data.normalizeIdentifier "btc" "BC1QX8HYAG2NVJJKKHDGJRSLWLYKM86PFLQNGEXAXN"
                    |> Expect.equal "bc1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn"
        , test "an uppercase ltc bech32 address becomes lowercase" <|
            \_ ->
                Data.normalizeIdentifier "ltc" "LTC1Q000HMA9YSNH7REF6CRN45448VNW8VLWAWLMXQE"
                    |> Expect.equal "ltc1q000hma9ysnh7ref6crn45448vnw8vlwawlmxqe"
        , test "a lowercase bech32 address stays as it is" <|
            \_ ->
                Data.normalizeIdentifier "btc" "bc1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn"
                    |> Expect.equal "bc1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn"
        , test "a mixed-case bech32 address is invalid and stays as it is" <|
            \_ ->
                Data.normalizeIdentifier "btc" "bC1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn"
                    |> Expect.equal "bC1qx8hyag2nvjjkkhdgjrslwlykm86pflqngexaxn"
        , test "a base58 address that starts like bech32 keeps its case" <|
            \_ ->
                Data.normalizeIdentifier "ltc" "LTC1A4GhnLkKbpfkaDyXHTeShP2YksUVgY"
                    |> Expect.equal "LTC1A4GhnLkKbpfkaDyXHTeShP2YksUVgY"
        , test "a base58 address keeps its case" <|
            \_ ->
                Data.normalizeIdentifier "btc" "1Archive1n2C579dMsAu3iC6tWzuQJz8dN"
                    |> Expect.equal "1Archive1n2C579dMsAu3iC6tWzuQJz8dN"
        , test "an uppercase btc bech32 prefix on another network keeps its case" <|
            \_ ->
                Data.normalizeIdentifier "ltc" "BC1QX8HYAG2NVJJKKHDGJRSLWLYKM86PFLQNGEXAXN"
                    |> Expect.equal "BC1QX8HYAG2NVJJKKHDGJRSLWLYKM86PFLQNGEXAXN"
        ]


isMixedCaseBech32Suite : Test
isMixedCaseBech32Suite =
    -- the same cases as graphsenselib's bech32_mixed_case
    describe "Util.Data.isMixedCaseBech32"
        [ test "a valid btc bech32 address with mixed case" <|
            \_ ->
                Data.isMixedCaseBech32 "Bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
                    |> Expect.equal True
        , test "a valid ltc bech32 address with mixed case" <|
            \_ ->
                Data.isMixedCaseBech32 "Ltc1q000hma9ysnh7ref6crn45448vnw8vlwawlmxqe"
                    |> Expect.equal True
        , test "not all lowercase" <|
            \_ ->
                Data.isMixedCaseBech32 "bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlh"
                    |> Expect.equal False
        , test "not all uppercase" <|
            \_ ->
                Data.isMixedCaseBech32 "BC1QXY2KGDYGJRSQTZQ2N0YRF2493P83KKFJHX0WLH"
                    |> Expect.equal False
        , test "not when the characters are wrong too" <|
            \_ ->
                Data.isMixedCaseBech32 "Bc1qxy2kgdygjrsqtzq2n0yrf2493p83kkfjhx0wlj"
                    |> Expect.equal False
        , test "not a base58 address that starts like bech32" <|
            \_ ->
                Data.isMixedCaseBech32 "LTC1A4GhnLkKbpfkaDyXHTeShP2YksUVgY"
                    |> Expect.equal False
        , test "a valid taproot (bech32m) address with mixed case" <|
            \_ ->
                Data.isMixedCaseBech32 "Bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
                    |> Expect.equal True
        ]
