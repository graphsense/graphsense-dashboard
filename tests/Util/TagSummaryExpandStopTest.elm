module Util.TagSummaryExpandStopTest exposing (suite)

{-| `expandStopConcept` decides which tagged addresses lose their expand
handles. The weights mirror two real prod addresses: a Bridgers (DEX) address,
where `defi_dex` dominates, and the Internet Archive donation address, which
carries weak third-party `gambling`/`market` mentions that must not count.
-}

import Api.Data
import Dict
import Expect
import Test exposing (Test, describe, test)
import Util.Pathfinder.TagSummary exposing (expandStopConcept)


summary : List ( String, Float ) -> Api.Data.TagSummary
summary cloud =
    { broadCategory = "entity"
    , tagCount = List.length cloud
    , tagCountIndirect = Nothing
    , bestActor = Nothing
    , bestLabel = Nothing
    , conceptTagCloud =
        cloud
            |> List.map (\( c, w ) -> ( c, { cnt = 1, weighted = w } ))
            |> Dict.fromList
    , labelSummary = Dict.empty
    }


suite : Test
suite =
    describe "Util.Pathfinder.TagSummary.expandStopConcept"
        [ test "a pooling service as the top concept stops the expand" <|
            \_ ->
                summary
                    [ ( "defi_dex", 0.467 )
                    , ( "financial_crime", 0.148 )
                    , ( "market", 0.074 )
                    , ( "wallet_service", 0.074 )
                    , ( "unknown", 0.015 )
                    ]
                    |> expandStopConcept
                    |> Expect.equal (Just "defi_dex")
        , test "weak service mentions under a non-service top concept do not" <|
            \_ ->
                summary
                    [ ( "organization", 0.429 )
                    , ( "donation", 0.328 )
                    , ( "gambling", 0.032 )
                    , ( "market", 0.016 )
                    ]
                    |> expandStopConcept
                    |> Expect.equal Nothing
        , test "a heavy service concept stops it even when not on top" <|
            \_ ->
                summary [ ( "organization", 0.45 ), ( "defi_bridge", 0.35 ), ( "user", 0.2 ) ]
                    |> expandStopConcept
                    |> Expect.equal (Just "defi_bridge")
        , test "`unknown` on top does not hide a service right below it" <|
            \_ ->
                summary [ ( "unknown", 0.6 ), ( "mixing_service", 0.2 ), ( "user", 0.2 ) ]
                    |> expandStopConcept
                    |> Expect.equal (Just "mixing_service")
        , test "an untagged summary stops nothing" <|
            \_ ->
                summary []
                    |> expandStopConcept
                    |> Expect.equal Nothing
        ]
