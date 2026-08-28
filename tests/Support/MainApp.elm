module Support.MainApp exposing
    ( App
    , expectEffect
    , html
    , initAt
    , initAtWithStats
    , model
    , step
    , steps
    , title
    , toUrl
    )

{-| Drives the whole dashboard headlessly — the real `Update.update`,
`Update.updateByUrl` and `View.view`, with `key = ()`.

This is the counterpart to `Support.App`, which stays inside the Pathfinder.
What only this one reaches is `Update.elm` itself: page routing, the statusbar,
notifications, dialogs, user settings persistence and the auth state machine.

**Why it builds the model instead of calling `Init.init`.** `Init.init` takes a
`Plugin.Model.Flags` record whose _shape_ is generated from whichever plugins
`config/Config.elm` registers — six on a developer machine, none in CI — and no
value of that type can be written portably. `initialModel` below therefore
mirrors the record `Init.elm` builds, with `Plugin.Model.emptyModelState` in
place of the plugin init.

That mirroring is the one maintenance cost. It is bounded: the compiler rejects
this module the moment a field is added to or removed from `Model`, so only
_values_ can drift, not the shape. If `Init.init` ever stops needing typed
plugin flags (see the note in `Model.Flags`), delete `initialModel` and call it
directly.

**What this cannot see:** the effects `Init.init` fires on boot — the statistics
request, the two taxonomy requests, the translation fetch and the plugin
commands. The harness starts one moment later, with the app built but nothing
requested. Assertions therefore belong on what the app does _with_ a response,
not on the boot request that asked for it.

-}

import Api.Data
import Browser
import Dict
import Expect exposing (Expectation)
import Html
import Init.Notification
import Init.Pathfinder
import Init.Search
import Init.Statusbar
import Model exposing (Auth(..), Effect(..), Model, Msg, Page(..))
import Model.Locale
import Plugin.Model
import RemoteData
import Support.Env as Env
import Test.Html.Query as Query
import Tuple exposing (first)
import Update
import Url exposing (Url)
import Util.ThemedSelectBox as TSelectBox
import View



-- THE APP UNDER TEST


{-| The model plus the effects the most recent step produced.
-}
type App
    = App
        { model_ : Model ()
        , effects_ : List Effect
        }


model : App -> Model ()
model (App app) =
    app.model_



-- SETUP


toUrl : String -> Url
toUrl path =
    ("https://example.com" ++ path)
        |> Url.fromString
        |> Maybe.withDefault
            { protocol = Url.Https
            , host = "example.com"
            , port_ = Nothing
            , path = "/"
            , query = Nothing
            , fragment = Nothing
            }


{-| Mirrors the record in `Init.elm`. See the module note for why it is copied
rather than called.
-}
initialModel : Url -> Model ()
initialModel url =
    { url = url
    , key = ()
    , config = Env.viewConfig
    , page = Home
    , search = Init.Search.initWithRecents (Init.Search.initSearchAddressAndTxs Nothing) []
    , pathfinder =
        Init.Pathfinder.init
            { snapToGrid = Nothing
            , highlightClusterFriends = Nothing
            , tracingMode = Nothing
            , avoidOverlapingNodes = Nothing
            , recentSearches = []
            }
            |> first
    , user =
        { apiKey = ""
        , auth = Unknown
        , hovercard = Nothing
        }
    , stats = RemoteData.NotAsked
    , capabilities = RemoteData.NotAsked
    , width = round Env.viewport.width
    , height = round Env.viewport.height
    , error = ""
    , statusbar = Init.Statusbar.init
    , dialog = Nothing
    , supportedTokens = Dict.empty
    , plugins = Plugin.Model.emptyModelState
    , notifications = Init.Notification.init
    , localeSelectBox = TSelectBox.init <| List.map first Model.Locale.locales
    , dirty = False
    , navbarSubMenu = Nothing
    , fileDragOver = False
    }


{-| The app opened at `path`, which is what `Main.main` does on boot: build the
model, then hand the URL to `Update.updateByUrl`.
-}
initAt : String -> App
initAt path =
    initAtWithStats RemoteData.NotAsked path


{-| The app opened at `path` with the statistics already in a given state.

`initAt` leaves them `NotAsked`, which is the boot state. Anything that behaves
differently once the statistics have settled -- URL parsing does, because
`Route.Pathfinder.parser` resolves network segments against them -- needs this instead.

-}
initAtWithStats : RemoteData.WebData Api.Data.Stats -> String -> App
initAtWithStats stats path =
    let
        url =
            toUrl path

        start =
            initialModel url

        ( routed, produced ) =
            Update.updateByUrl Env.updateConfig url { start | stats = stats }
    in
    App { model_ = routed, effects_ = produced }



-- DRIVING


{-| One `Update.update`, with the config refreshed from the model exactly as
`Main.main` does before every update.
-}
step : Msg -> App -> App
step msg (App app) =
    let
        config =
            { locale = app.model_.config.locale
            , size = app.model_.config.size
            , abuseConcepts = app.model_.config.abuseConcepts
            , allConcepts = app.model_.config.allConcepts
            }

        ( next, produced ) =
            Update.update config msg app.model_
    in
    App { model_ = next, effects_ = produced }


steps : List Msg -> App -> App
steps msgs app =
    List.foldl step app msgs



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
        NavLoadEffect url ->
            "NavLoadEffect " ++ url

        NavPushUrlEffect url ->
            "NavPushUrlEffect " ++ url

        NavBackEffect _ ->
            "NavBackEffect"

        GetContentsElementEffect ->
            "GetContentsElementEffect"

        LocaleEffect _ ->
            "LocaleEffect"

        SearchEffect _ _ ->
            "SearchEffect"

        PathfinderEffect _ ->
            "PathfinderEffect"

        ApiEffect _ ->
            "ApiEffect"

        PluginEffect _ ->
            "PluginEffect"

        PortsConsoleEffect _ ->
            "PortsConsoleEffect"

        CmdEffect _ ->
            "CmdEffect"

        LogoutEffect ->
            "LogoutEffect"

        SaveUserSettingsEffect _ ->
            "SaveUserSettingsEffect"

        NotificationEffect _ ->
            "NotificationEffect"

        PostponeUpdateByUrlEffect _ ->
            "PostponeUpdateByUrlEffect"



-- THE RENDERED PAGE


document : App -> Browser.Document Msg
document (App app) =
    View.view app.model_.config app.model_


{-| The browser tab title, which the app assembles from the page and plugins.
-}
title : App -> String
title =
    document >> .title


{-| The real `View.view`, ready for `Test.Html.Query`/`Selector`.
-}
html : App -> Query.Single Msg
html =
    document
        >> .body
        >> Html.div []
        >> Query.fromHtml
