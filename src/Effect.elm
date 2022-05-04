module Effect exposing (perform)

import Api
import Api.Request.General
import Bounce
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Http
import Locale.Effect
import Model exposing (Auth(..), Effect(..), Msg(..))
import Search.Effect as Search
import Task


perform : Nav.Key -> String -> Effect -> Cmd Msg
perform key apiKey effect =
    case effect of
        NoEffect ->
            Cmd.none

        NavLoadEffect url ->
            Nav.load url

        NavPushUrlEffect url ->
            Nav.pushUrl key url

        GetStatisticsEffect ->
            Api.Request.General.getStatistics
                |> Api.send BrowserGotStatistics

        GetElementEffect { id, msg } ->
            Dom.getElement id
                |> Task.attempt msg

        BatchedEffects effs ->
            List.map (perform key apiKey) effs
                |> Cmd.batch

        LocaleEffect eff ->
            Locale.Effect.perform eff
                |> Cmd.map LocaleMsg

        SearchEffect Search.NoEffect ->
            Cmd.none

        SearchEffect (Search.BatchEffect eff) ->
            List.map (SearchEffect >> perform key apiKey) eff
                |> Cmd.batch

        SearchEffect (Search.SearchEffect { query, currency, limit, toMsg }) ->
            Api.Request.General.search query currency limit
                |> Api.withTracker "search"
                |> withAuthorization apiKey
                |> Api.sendAndAlsoReceiveHeaders BrowserGotResponseWithHeaders effect (toMsg >> SearchMsg)

        SearchEffect Search.CancelEffect ->
            Http.cancel "search"
                |> Cmd.map SearchMsg

        SearchEffect (Search.BounceEffect delay msg) ->
            Bounce.delay delay msg
                |> Cmd.map SearchMsg


withAuthorization : String -> Api.Request a -> Api.Request a
withAuthorization apiKey request =
    Api.withHeader "Authorization" apiKey request
