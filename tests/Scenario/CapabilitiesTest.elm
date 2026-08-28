module Scenario.CapabilitiesTest exposing (suite)

{-| A deep link loads an address or tx right away, and that load decides per
network which optional requests it may fire (neighbors, conversions, tag
summaries). Those decisions read the `/capabilities` answer, so the load waits
for it — and only the load: plain pages do not.

Once received, the answer must survive the two places that rebuild the
Pathfinder model from scratch: "Restart" and opening a saved graph.

-}

import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Http
import Json.Decode
import Model exposing (Effect(..), Msg(..), Page(..))
import Model.NetworkCapabilities as NetworkCapabilities
import Msg.Pathfinder as Pathfinder
import RemoteData
import Support.MainApp as App
import Test exposing (Test, describe, test)


isPostpone : Effect -> Bool
isPostpone effect =
    case effect of
        PostponeUpdateByUrlEffect _ ->
            True

        _ ->
            False


withStats : (Api.Data.Stats -> Expectation) -> Expectation
withStats f =
    case Json.Decode.decodeString Api.Data.statsDecoder Fixture.stats of
        Ok stats ->
            f stats

        Err error ->
            Expect.fail ("the stats fixture did not decode: " ++ Json.Decode.errorToString error)


btcWithoutRelations : Api.Data.Capabilities
btcWithoutRelations =
    { networks = [ { network = "btc", disabled = [ "relations" ] } ] }


capabilitiesFailed : Msg
capabilitiesFailed =
    Err ( Http.BadStatus 404, Dict.empty, Effect.Api.GetCapabilitiesEffect BrowserGotCapabilities )
        |> BrowserGotResponseWithHeaders Nothing


suite : Test
suite =
    describe "capabilities"
        [ test "an address deep link waits for the /capabilities answer" <|
            \_ ->
                withStats <|
                    \stats ->
                        App.initAtWithStats (RemoteData.Success stats) "/pathfinder/btc/address/1Archive1n2C579dMsAu3iC6tWzuQJz8dN"
                            |> App.expectEffect "PostponeUpdateByUrlEffect" isPostpone
        , test "the bare pathfinder page does not wait" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.model
                    |> .page
                    |> Expect.equal Pathfinder
        , test "a backend without the endpoint (404) releases the deep link" <|
            \_ ->
                withStats <|
                    \stats ->
                        App.initAtWithStats (RemoteData.Success stats) "/pathfinder/btc/address/1Archive1n2C579dMsAu3iC6tWzuQJz8dN"
                            |> App.step capabilitiesFailed
                            |> App.step (RuntimePostponedUpdateByUrl (App.toUrl "/pathfinder/btc/address/1Archive1n2C579dMsAu3iC6tWzuQJz8dN"))
                            |> App.expectEffect "no PostponeUpdateByUrlEffect" (isPostpone >> not)
        , test "the answer reaches the pathfinder gates" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (BrowserGotCapabilities btcWithoutRelations)
                    |> App.model
                    |> .pathfinder
                    |> .networkCapabilities
                    |> (\caps -> NetworkCapabilities.supports NetworkCapabilities.Relations caps "btc")
                    |> Expect.equal False
        , test "restart keeps the answer" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (BrowserGotCapabilities btcWithoutRelations)
                    |> App.step (PathfinderMsg Pathfinder.UserClickedRestartYes)
                    |> App.model
                    |> .pathfinder
                    |> .networkCapabilities
                    |> (\caps -> NetworkCapabilities.supports NetworkCapabilities.Relations caps "btc")
                    |> Expect.equal False
        ]
