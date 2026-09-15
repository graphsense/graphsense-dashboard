module Util.DataTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Util.Data as Data


suite : Test
suite =
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
