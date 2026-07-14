module UtilTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util exposing (foldLigatures)


suite : Test
suite =
    describe "foldLigatures"
        [ test "expands the ff ligature in a tx hash pasted from a PDF" <|
            \_ ->
                foldLigatures "d2b79bc0e7001bdfc66cf6a80f5ﬀd1d2702097b725fca2bad5ea0c0e31c020f"
                    |> Expect.equal "d2b79bc0e7001bdfc66cf6a80f5ffd1d2702097b725fca2bad5ea0c0e31c020f"
        , test "expands all typographic f-ligatures" <|
            \_ ->
                foldLigatures "ﬀﬁﬂﬃﬄﬅﬆ"
                    |> Expect.equal "fffiflffifflstst"
        , test "preserves case of ordinary characters (base58 is case-sensitive)" <|
            \_ ->
                foldLigatures "1BoatSLRHtKNngkdXEeobR76b53LETtpyT"
                    |> Expect.equal "1BoatSLRHtKNngkdXEeobR76b53LETtpyT"
        , test "leaves plain ascii untouched" <|
            \_ ->
                foldLigatures "0xDeadBeef"
                    |> Expect.equal "0xDeadBeef"
        ]
