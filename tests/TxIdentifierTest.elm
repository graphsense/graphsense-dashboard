module TxIdentifierTest exposing (suite)

{-| Deep-link tx identifiers: the sub-tx markers "\_T<n>"/"\_I<n>" are
case-SENSITIVE on the API (lowercase "\_t2" is a 400), and served tx ids
carry no "0x". The tx route handler must therefore normalize only the hex
part — the ADDRESS normalizer (whole-string toLower + ensure0x) broke every
tx deep link with a sub-tx suffix.
-}

import Expect
import Model.Tx as Tx
import Test exposing (Test, describe, test)
import Util.Data as Data
import Util.ExternalLinks as ExternalLinks


suite : Test
suite =
    describe "normalizeTxIdentifier"
        [ test "explorer identifiers reject non-hex first characters and empty hashes" <|
            \_ ->
                [ "", "0x", "0X", "?abc", "/abc", "#abc", "_T2", "abc?x=1" ]
                    |> List.map Tx.parseTxIdentifier
                    |> Expect.equal (List.repeat 8 Nothing)
        , test "explorer identifiers accept either hex prefix without changing the leg suffix" <|
            \_ ->
                [ "ABC_T2", "0xABC_T2", "0XABC_T2" ]
                    |> List.map (ExternalLinks.getBlockExplorerTransactionLinks "eth" >> List.head)
                    |> Expect.equal (List.repeat 3 (Just ( "https://etherscan.io/tx/0xABC/advanced#eventlog#2", "Etherscan" )))
        , test "non-bridge BTC, Tron and internal-call links keep their hash and anchors" <|
            \_ ->
                [ ( "btc", "00ABCDEF" ), ( "trx", "00ABCDEF_T2" ), ( "eth", "0X00ABCDEF_I0" ) ]
                    |> List.map (\( network, identifier ) -> ExternalLinks.getBlockExplorerTransactionLinks network identifier |> List.head)
                    |> Expect.equal
                        [ Just ( "https://www.oklink.com/btc/tx/00ABCDEF", "Oklink" )
                        , Just ( "https://tronscan.org/#/transaction/00ABCDEF", "Tronscan" )
                        , Just ( "https://etherscan.io/tx/0x00ABCDEF/advanced#internal", "Etherscan" )
                        ]
        , test "a network without an explorer produces no links" <|
            \_ ->
                ExternalLinks.getBlockExplorerTransactionLinks "unknown" "abc_T2"
                    |> Expect.equal []
        , test "strips 0x and preserves the case-sensitive sub-tx marker" <|
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
