module Scenario.CapabilitiesTest exposing (suite)

{-| A deep link loads an address or tx right away, and that load decides per
network which optional requests it may fire (neighbors, conversions, tag
summaries). Those decisions read the `/capabilities` answer, so the load waits
for it — and only the load: plain pages do not.

Once received, the answer must survive the two places that rebuild the
Pathfinder model from scratch: "Restart" and opening a saved graph.

A graph handed over to a fresh tab ("Open in new tab" on a multi-selection,
Ctrl+D) or a `?import=` deep link arrives at boot, before the answer, and
loading it fires the same per-network requests -- so it waits the same way.

-}

import Api.Data
import Dict
import Effect.Api
import Expect exposing (Expectation)
import Fixtures.Api as Fixture
import Http
import Json.Decode
import Json.Encode
import Model exposing (Effect(..), Msg(..), Page(..))
import Model.NetworkCapabilities as NetworkCapabilities
import Msg.Pathfinder as Pathfinder
import RemoteData
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)


isPostpone : Effect -> Bool
isPostpone effect =
    case effect of
        PostponeUpdateByUrlEffect _ ->
            True

        _ ->
            False


isPostponeDeserialize : Effect -> Bool
isPostponeDeserialize effect =
    case effect of
        PostponeDeserializeEffect _ ->
            True

        _ ->
            False


{-| A saved graph of two btc addresses, as `Encode.Pathfinder` writes it and
as the new-tab hand-over parks it.
-}
twoAddressesGs : ( String, Json.Encode.Value )
twoAddressesGs =
    let
        address id x =
            Json.Encode.list identity
                [ Json.Encode.list Json.Encode.string [ "btc", id ]
                , Json.Encode.float x
                , Json.Encode.float 0
                , Json.Encode.bool False
                ]
    in
    ( "selection.gs"
    , Json.Encode.list identity
        [ Json.Encode.string "pathfinder"
        , Json.Encode.string "1"
        , Json.Encode.string "selection"
        , Json.Encode.list identity
            [ address "1Archive1n2C579dMsAu3iC6tWzuQJz8dN" 0
            , address "1BoatSLRHtKNngkdXEeobR76b53LETtpyT" 200
            ]
        , Json.Encode.list identity []
        , Json.Encode.list identity []
        , Json.Encode.list identity []
        ]
    )


loadedAddresses : App -> Int
loadedAddresses app =
    App.model app |> .pathfinder |> .network |> .addresses |> Dict.size


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
        , test "an opened graph waits for the /capabilities answer" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (BrowserGotDeserializedGS twoAddressesGs)
                    |> App.expectEffect "PostponeDeserializeEffect" isPostponeDeserialize
        , test "an opened graph is not loaded before the answer" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (BrowserGotDeserializedGS twoAddressesGs)
                    |> loadedAddresses
                    |> Expect.equal 0
        , test "the answer releases the opened graph" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (BrowserGotCapabilities btcWithoutRelations)
                    |> App.step (BrowserGotDeserializedGS twoAddressesGs)
                    |> loadedAddresses
                    |> Expect.equal 2
        , test "a backend without the endpoint (404) releases the opened graph" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step capabilitiesFailed
                    |> App.step (BrowserGotDeserializedGS twoAddressesGs)
                    |> loadedAddresses
                    |> Expect.equal 2
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
