module TxIdentifierTest exposing (suite)

{-| Deep-link tx identifiers: the sub-tx markers "\_T<n>"/"\_I<n>" are
case-SENSITIVE on the API (lowercase "\_t2" is a 400), and served tx ids
carry no "0x". The tx route handler must therefore normalize only the hex
part — the ADDRESS normalizer (whole-string toLower + ensure0x) broke every
tx deep link with a sub-tx suffix.
-}

import Expect
import Test exposing (Test, describe, test)
import Util.Data as Data


suite : Test
suite =
    describe "normalizeTxIdentifier"
        [ test "strips 0x and preserves the case-sensitive sub-tx marker" <|
            \_ ->
                Data.normalizeTxIdentifier "arb" "0xE9EF1D038D150059C4CA9BDCD4A460772571BFFD142B5AF8FBC30776FA2789BC_T2"
                    |> Expect.equal "e9ef1d038d150059c4ca9bdcd4a460772571bffd142b5af8fbc30776fa2789bc_T2"
        , test "plain hash: lowercased, 0x stripped" <|
            \_ ->
                Data.normalizeTxIdentifier "eth" "0xABCDEF"
                    |> Expect.equal "abcdef"
        , test "internal-call marker preserved" <|
            \_ ->
                Data.normalizeTxIdentifier "eth" "0xABC_I0"
                    |> Expect.equal "abc_I0"
        , test "non-EVM networks unchanged" <|
            \_ ->
                Data.normalizeTxIdentifier "btc" "AbCdEf"
                    |> Expect.equal "AbCdEf"
        ]
