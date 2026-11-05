module Scenario.ActorDialogTest exposing (suite)

{-| The actor dialog: a click on an actor (label, chip, the tooltip's
"learn more") or an actor search result opens a dialog with everything the
API has on the actor. The actor tooltip keeps a long jurisdiction list short.
-}

import Api.Data
import Dict
import Effect.Api
import Effect.Pathfinder
import Expect
import Html.Attributes
import Html.Styled
import Model exposing (Effect(..), Msg(..))
import Msg.Pathfinder as Pathfinder
import Support.Env as Env
import Support.MainApp as App exposing (App)
import Test exposing (Test, describe, test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Util.Tooltip
import Util.TooltipType exposing (TooltipType(..))
import View.Locale as Locale


{-| An exchange with many jurisdictions, as Binance has.
-}
binance : Api.Data.Actor
binance =
    { categories = [ { id = "exchange", label = "Exchange" } ]
    , context =
        Just
            { coingeckoIds = [ "binancecoin" ]
            , defilamaIds = []
            , githubOrganisation = Just ""
            , images = []
            , legalName = Just "Binance Holdings Ltd."
            , refs = [ "https://en.wikipedia.org/wiki/Binance" ]
            , twitterHandle = Just "BinanceUS, binance"
            , uris = [ "https://www.binance.com" ]
            }
    , id = "binance"
    , jurisdictions = List.map (\c -> { id = c, label = "Country " ++ c }) jurisdictionIds
    , label = "Binance"
    , nrTags = Just 1234
    , uri = "binance.com"
    }


jurisdictionIds : List String
jurisdictionIds =
    [ "AE", "BH", "BR", "FR", "IT", "JP", "KZ", "LT", "PL", "ES" ]


dialog : App -> Query.Single Msg
dialog =
    App.html >> Query.find [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-actor-dialog") ]


hasNoDialog : App -> Expect.Expectation
hasNoDialog =
    App.html >> Query.findAll [ Selector.attribute (Html.Attributes.attribute "data-testid" "gs-actor-dialog") ] >> Query.count (Expect.equal 0)


isActorFetch : String -> Effect -> Bool
isActorFetch actorId eff =
    case eff of
        PathfinderEffect (Effect.Pathfinder.ApiEffect (Effect.Api.GetActorEffect e _)) ->
            e.actorId == actorId

        _ ->
            False


withActorLoaded : App -> App
withActorLoaded =
    App.mapModel
        (\m ->
            let
                pf =
                    m.pathfinder
            in
            { m | pathfinder = { pf | actors = Dict.insert binance.id binance pf.actors } }
        )


suite : Test
suite =
    describe "Actor dialog"
        [ test "an actor search result fetches the actor" <|
            \_ ->
                App.initAt "/pathfinder/actor/binance"
                    |> App.expectEffect "the actor request" (isActorFetch "binance")
        , test "the fetched actor opens the dialog with every jurisdiction" <|
            \_ ->
                App.initAt "/pathfinder/actor/binance"
                    |> App.step (PathfinderMsg (Pathfinder.BrowserGotActorForDialog "binance" binance))
                    |> dialog
                    |> Query.has
                        (Selector.text "Binance"
                            :: Selector.text "Binance Holdings Ltd."
                            :: List.map (\c -> Selector.text ("Country " ++ c)) jurisdictionIds
                        )
        , test "the dialog links the actor's context" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (PathfinderMsg (Pathfinder.BrowserGotActorForDialog "binance" binance))
                    |> dialog
                    |> Query.has
                        [ Selector.attribute (Html.Attributes.href "https://binance.com")
                        , Selector.attribute (Html.Attributes.href "https://www.binance.com")
                        , Selector.attribute (Html.Attributes.href "https://x.com/BinanceUS")
                        , Selector.attribute (Html.Attributes.href "https://x.com/binance")
                        , Selector.text (Locale.int Env.viewConfig.locale 1234)
                        ]
        , test "a click on a loaded actor opens the dialog at once, without a request" <|
            \_ ->
                let
                    app =
                        App.initAt "/pathfinder"
                            |> withActorLoaded
                            |> App.step (PathfinderMsg (Pathfinder.UserClickedActor "binance"))
                in
                Expect.all
                    [ dialog >> Query.has [ Selector.text "Binance" ]
                    , App.expectNoEffect "an actor request" (isActorFetch "binance")
                    ]
                    app
        , test "a click on an actor not loaded yet fetches it and waits" <|
            \_ ->
                let
                    app =
                        App.initAt "/pathfinder"
                            |> App.step (PathfinderMsg (Pathfinder.UserClickedActor "binance"))
                in
                Expect.all
                    [ hasNoDialog
                    , App.expectEffect "the actor request" (isActorFetch "binance")
                    ]
                    app
        , test "the actor tooltip shows three jurisdictions and counts the rest" <|
            \_ ->
                let
                    pathfinder =
                        App.initAt "/pathfinder" |> withActorLoaded |> App.model |> .pathfinder

                    tooltip =
                        Util.Tooltip.view Env.viewConfig pathfinder (ActorDetails "binance")
                            |> Html.Styled.div []
                            |> Html.Styled.toUnstyled
                            |> Query.fromHtml
                in
                Expect.all
                    [ Query.has [ Selector.text "Country AE", Selector.text "Country BR", Selector.text (Locale.interpolated Env.viewConfig.locale "and-more" [ "7" ]) ]
                    , Query.hasNot [ Selector.text "Country FR" ]
                    , Query.find [ Selector.attribute (Html.Attributes.attribute "role" "button") ]
                        >> Event.simulate Event.click
                        >> Event.expect (Pathfinder.UserClickedActor "binance")
                    ]
                    tooltip
        , test "a blank context value leaves its row out" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (PathfinderMsg (Pathfinder.BrowserGotActorForDialog "binance" binance))
                    |> dialog
                    |> Query.hasNot [ Selector.text "GitHub" ]
        , test "references, CoinGecko and DefiLlama are not shown" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (PathfinderMsg (Pathfinder.BrowserGotActorForDialog "binance" binance))
                    |> dialog
                    |> Expect.all
                        [ Query.hasNot [ Selector.text "https://en.wikipedia.org/wiki/Binance" ]
                        , Query.hasNot [ Selector.attribute (Html.Attributes.href "https://www.coingecko.com/en/coins/binancecoin") ]
                        , Query.hasNot [ Selector.text "CoinGecko" ]
                        ]
        , test "social media links read X.com, not the handle" <|
            \_ ->
                App.initAt "/pathfinder"
                    |> App.step (PathfinderMsg (Pathfinder.BrowserGotActorForDialog "binance" binance))
                    |> dialog
                    |> Expect.all
                        [ Query.findAll [ Selector.attribute (Html.Attributes.href "https://x.com/BinanceUS") ]
                            >> Query.first
                            >> Query.has [ Selector.text "X.com" ]
                        , Query.hasNot [ Selector.text "BinanceUS" ]
                        ]
        ]
