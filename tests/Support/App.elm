module Support.App exposing
    ( App
    , apiEffects
    , effects
    , expectEffect
    , html
    , init
    , initAt
    , model
    , respond
    , step
    , steps
    )

{-| Drives the Pathfinder headlessly: real `Update.Pathfinder.update`, real
`View.Pathfinder.view`, no browser and no ports.

Effects are plain data, and every `Effect.Api.Effect` carries the function that
turns a response into a `Msg`, so an API round trip is just "find the effect,
hand it a fixture, feed the resulting Msg back in":

    init
        |> step (UserPushedAddress someId)
        |> respond
            (\eff ->
                case eff of
                    Effect.Api.GetAddressEffect _ toMsg ->
                        Just (toMsg addressFixture)

                    _ ->
                        Nothing
            )
        |> html
        |> Query.has [ Selector.text "1Archive" ]

That covers everything between a user action and the rendered DOM — the part a
browser driver would otherwise have to click through.

**Why Pathfinder and not the whole app:** `Init.init` needs a
`Plugin.Model.Flags` record whose _shape_ is generated from whichever plugins
`config/Config.elm` registers — six on a developer machine, none in CI. There is
no value of that type a test can construct portably. The Pathfinder entry points
take `Plugin.Update.empty` / `Plugin.Model.emptyModelState` instead, so these
tests behave identically everywhere. That also keeps plugin behaviour out of the
picture, which is what you want when testing the core.

-}

import Effect.Api
import Effect.Pathfinder exposing (Effect(..))
import Expect exposing (Expectation)
import Html.Styled
import Init.Pathfinder
import Model.Pathfinder exposing (Model)
import Msg.Pathfinder exposing (Msg)
import Plugin.Model
import Route.Pathfinder exposing (Route)
import Support.Env as Env
import Test.Html.Query as Query
import Update.Pathfinder
import View.Pathfinder



-- THE APP UNDER TEST


{-| The model plus the effects the most recent step produced.
-}
type App
    = App
        { model_ : Model
        , effects_ : List Effect
        }


model : App -> Model
model (App app) =
    app.model_


effects : App -> List Effect
effects (App app) =
    app.effects_



-- SETUP


{-| An empty graph, as the user gets on a bare `/pathfinder`.
-}
init : App
init =
    Init.Pathfinder.init
        { snapToGrid = Nothing
        , highlightClusterFriends = Nothing
        , tracingMode = Nothing
        , avoidOverlapingNodes = Nothing
        , recentSearches = []
        }
        |> Tuple.first
        |> (\m -> App { model_ = m, effects_ = [] })


{-| The same, but deep-linked to `route` — what opening a Pathfinder URL does.
-}
initAt : Route -> App
initAt route =
    let
        (App app) =
            init

        ( routed, produced ) =
            Update.Pathfinder.updateByRoute Env.updatePlugins Env.updateConfig route app.model_
    in
    App { model_ = routed, effects_ = produced }



-- DRIVING


{-| One `Update.Pathfinder.update`, replacing the recorded effects with the new
ones.
-}
step : Msg -> App -> App
step msg (App app) =
    let
        ( next, produced ) =
            Update.Pathfinder.update Env.updatePlugins Env.updateConfig msg app.model_
    in
    App { model_ = next, effects_ = produced }


steps : List Msg -> App -> App
steps msgs app =
    List.foldl step app msgs


{-| Answers the pending API requests.

The callback sees every API effect the last step produced and returns the `Msg`
that response would produce — typically by matching the effect constructor and
applying the continuation it carries to a fixture. Effects it ignores are
dropped, as they would be in a test that does not care about them.

-}
respond : (Effect.Api.Effect Msg -> Maybe Msg) -> App -> App
respond toMsg app =
    apiEffects app
        |> List.filterMap toMsg
        |> (\msgs -> steps msgs app)


{-| Every API request the last step asked for, including the ones nested inside
batched effects.
-}
apiEffects : App -> List (Effect.Api.Effect Msg)
apiEffects (App app) =
    List.concatMap fromEffect app.effects_


fromEffect : Effect -> List (Effect.Api.Effect Msg)
fromEffect eff =
    case eff of
        ApiEffect apiEff ->
            [ apiEff ]

        BatchEffect batched ->
            List.concatMap fromEffect batched

        _ ->
            []



-- ASSERTIONS


{-| Asserts that the last step produced at least one effect matching
`predicate`; names the effects it did produce when it did not.
-}
expectEffect : String -> (Effect -> Bool) -> App -> Expectation
expectEffect description predicate (App app) =
    List.any predicate app.effects_
        |> Expect.equal True
        |> Expect.onFail
            ("expected an effect matching \""
                ++ description
                ++ "\", got ["
                ++ (app.effects_ |> List.map name |> String.join ", ")
                ++ "]"
            )


name : Effect -> String
name eff =
    case eff of
        NavPushRouteEffect _ ->
            "NavPushRouteEffect"

        PluginEffect _ ->
            "PluginEffect"

        ApiEffect _ ->
            "ApiEffect"

        BatchEffect batched ->
            "BatchEffect [" ++ (batched |> List.map name |> String.join ", ") ++ "]"

        CmdEffect _ ->
            "CmdEffect"

        SearchEffect _ ->
            "SearchEffect"

        ErrorEffect _ ->
            "ErrorEffect"

        RepositionTooltipEffect ->
            "RepositionTooltipEffect"

        PostponeUpdateByRouteEffect _ ->
            "PostponeUpdateByRouteEffect"

        ShowNotificationEffect _ ->
            "ShowNotificationEffect"

        InternalEffect _ ->
            "InternalEffect"

        TransactionFilterEffect _ ->
            "TransactionFilterEffect"

        TooltipEffect _ ->
            "TooltipEffect"

        StatusbarLogEffect _ _ ->
            "StatusbarLogEffect"



-- THE RENDERED PAGE


{-| The real `View.Pathfinder.view`, ready for `Test.Html.Query`/`Selector`.

Plugin views get `Plugin.Model.emptyModelState`, so they contribute nothing and
the output does not depend on the local plugin configuration.

-}
html : App -> Query.Single Msg
html (App app) =
    View.Pathfinder.view Env.viewPlugins Plugin.Model.emptyModelState Env.viewConfig app.model_
        |> .contents
        |> Html.Styled.div []
        |> Html.Styled.toUnstyled
        |> Query.fromHtml
