module Model.Pathfinder.ConversionEdgeTest exposing (suite)

import Api.Data
import Expect
import Init.Pathfinder.ConversionEdge as ConversionEdge
import Init.Pathfinder.Id as Id
import Model.Pathfinder.ConversionEdge as ConversionEdge exposing (ConversionEdge)
import Support.SwapFixture exposing (accountTx, dexSwap, inputLegId, outputLegId, settlement, swapper)
import Test exposing (Test, describe, test)


stranger : String
stranger =
    "0x00000000000000000000000000000000000000ff"


{-| The swap's edge whose leg transactions move value between `legAddress` and
the settlement contract.
-}
edgeWith : String -> Api.Data.ExternalConversion -> ConversionEdge
edgeWith legAddress raw =
    ConversionEdge.init raw
        ( Id.init "bnb" inputLegId, Id.init "bnb" outputLegId )
        ( Id.init "bnb" swapper, Id.init "bnb" swapper )
        (accountTx inputLegId legAddress settlement)
        (accountTx outputLegId settlement legAddress)


edge : Api.Data.ExternalConversion -> ConversionEdge
edge =
    edgeWith swapper


{-| The conversion's `fromAddress`/`toAddress` is on neither leg transaction.
-}
mismatchedEdge : Api.Data.ExternalConversion -> ConversionEdge
mismatchedEdge =
    edgeWith stranger


usd6 : List Api.Data.Rate
usd6 =
    [ { code = "usd", value = 6 } ]


suite : Test
suite =
    describe "Model.Pathfinder.ConversionEdge leg values"
        [ test "the conversion's own quote wins over the leg transaction's" <|
            \_ ->
                edge { dexSwap | toAmountFiatValues = Just usd6 }
                    |> ConversionEdge.outputValues
                    |> Expect.equal { fiatValues = usd6, value = 1 }
        , test "without a quote the leg transaction's own values are untouched" <|
            \_ ->
                edge dexSwap
                    |> ConversionEdge.outputValues
                    |> Expect.equal { fiatValues = [], value = 1 }
        , test "the input leg's own quote wins too" <|
            \_ ->
                edge { dexSwap | fromAmountFiatValues = Just usd6 }
                    |> ConversionEdge.inputValues
                    |> Expect.equal { fiatValues = usd6, value = 1 }
        , test "without a quote the input leg's own values are untouched" <|
            \_ ->
                edge dexSwap
                    |> ConversionEdge.inputValues
                    |> Expect.equal { fiatValues = [], value = 1 }
        , test "an address-mismatched leg carries no fiat beside its zero amount" <|
            \_ ->
                mismatchedEdge { dexSwap | fromAmountFiatValues = Just usd6 }
                    |> ConversionEdge.inputValues
                    |> .fiatValues
                    |> Expect.equal []
        , test "an address-mismatched output leg carries no fiat either" <|
            \_ ->
                mismatchedEdge { dexSwap | toAmountFiatValues = Just usd6 }
                    |> ConversionEdge.outputValues
                    |> .fiatValues
                    |> Expect.equal []
        ]
