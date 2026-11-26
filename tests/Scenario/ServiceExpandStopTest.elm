module Scenario.ServiceExpandStopTest exposing (suite)

{-| An address tagged as a fund-pooling service (DEX, bridge, mixer, ...) does
not expand: the "next" tx there is the service's latest unrelated customer,
not a continuation of the trace. The click explains instead of expanding.

Driven through the real tag summary response, so this also pins that the
summary actually reaches the address. The control is the spec's own tag
summary (Internet Archive, `organization`), which still expands.

-}

import Api.Data
import Dict
import Effect.Api
import Effect.Pathfinder
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Json.Decode
import Model.Direction exposing (Direction(..))
import Model.Pathfinder.Error exposing (Error(..), InfoError(..))
import Model.Pathfinder.Id exposing (Id)
import Msg.Pathfinder exposing (Msg(..))
import Route.Pathfinder as Route
import Support.App as App exposing (App)
import Test exposing (Test, describe, test)


anchor : Id
anchor =
    ( "btc", "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" )


decode : Json.Decode.Decoder a -> String -> (a -> Expectation) -> Expectation
decode decoder json f =
    case Json.Decode.decodeString decoder json of
        Ok value ->
            f value

        Err error ->
            Expect.fail ("fixture did not decode: " ++ Json.Decode.errorToString error)


{-| The spec's tag summary with its concepts swapped for a DEX's.
-}
asDex : Api.Data.TagSummary -> Api.Data.TagSummary
asDex ts =
    { ts
        | conceptTagCloud =
            Dict.fromList
                [ ( "defi_dex", { cnt = 2, weighted = 0.47 } )
                , ( "market", { cnt = 1, weighted = 0.07 } )
                ]
    }


{-| Opens the anchor, answers its address and tag summary requests, then
clicks the backward expand handle.
-}
clickExpandWith : Api.Data.TagSummary -> Api.Data.Address -> App
clickExpandWith ts address =
    Route.addressRoute { network = "btc", address = Tuple.second anchor }
        |> App.initAt
        |> App.respond
            (\eff ->
                case eff of
                    Effect.Api.GetAddressEffect _ toMsg ->
                        Just (toMsg address)

                    _ ->
                        Nothing
            )
        |> App.respond
            (\eff ->
                case eff of
                    Effect.Api.GetAddressTagSummaryEffect _ toMsg ->
                        Just (toMsg ts)

                    _ ->
                        Nothing
            )
        |> App.step (UserClickedAddressExpandHandle anchor Incoming)


withFixtures : (Api.Data.TagSummary -> Api.Data.Address -> Expectation) -> Expectation
withFixtures f =
    decode Api.Data.tagSummaryDecoder Fixture.tagSummary <|
        \ts ->
            decode Api.Data.addressDecoder Fixture.address (f ts)


isServiceNotice : Effect.Pathfinder.Effect -> Bool
isServiceNotice eff =
    case eff of
        Effect.Pathfinder.ErrorEffect (InfoError (TxTracingThroughService id _)) ->
            id == anchor

        Effect.Pathfinder.BatchEffect batched ->
            List.any isServiceNotice batched

        _ ->
            False


serviceStop : App -> Maybe String
serviceStop app =
    (App.model app).network.addresses
        |> Dict.get anchor
        |> Maybe.andThen .serviceStop


suite : Test
suite =
    describe "expanding a pooling-service address"
        [ test "the tag summary marks the address" <|
            \_ ->
                withFixtures <|
                    \ts address ->
                        clickExpandWith (asDex ts) address
                            |> serviceStop
                            |> Expect.equal (Just "defi_dex")
        , test "asks the API for nothing" <|
            \_ ->
                withFixtures <|
                    \ts address ->
                        clickExpandWith (asDex ts) address
                            |> App.apiEffects
                            |> List.length
                            |> Expect.equal 0
        , test "explains why" <|
            \_ ->
                withFixtures <|
                    \ts address ->
                        clickExpandWith (asDex ts) address
                            |> App.expectEffect "the service notice" isServiceNotice
        , describe "an organization (the spec's Internet Archive summary)"
            [ test "still expands" <|
                \_ ->
                    withFixtures <|
                        \ts address ->
                            clickExpandWith ts address
                                |> App.apiEffects
                                |> List.isEmpty
                                |> Expect.equal False
            , test "without the notice" <|
                \_ ->
                    withFixtures <|
                        \ts address ->
                            clickExpandWith ts address
                                |> App.expectNoEffect "the service notice" isServiceNotice
            ]
        ]
