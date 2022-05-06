module Effect exposing (n, perform)

import Api
import Api.Request.Addresses
import Api.Request.General
import Bounce
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Search as Search
import Effect.Store as Store
import Http
import Model exposing (Auth(..), Effect(..), Msg(..))
import Task


n : m -> ( m, List eff )
n m =
    ( m, [] )


perform : Nav.Key -> String -> Effect -> Cmd Msg
perform key apiKey effect =
    case effect of
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

        LocaleEffect eff ->
            Locale.perform eff
                |> Cmd.map LocaleMsg

        GraphEffect eff ->
            Graph.perform eff
                |> Cmd.map GraphMsg

        SearchEffect (Search.SearchEffect { query, currency, limit, toMsg }) ->
            Api.Request.General.search query currency limit
                |> Api.withTracker "search"
                |> send apiKey effect (toMsg >> SearchMsg)

        SearchEffect Search.CancelEffect ->
            Http.cancel "search"
                |> Cmd.map SearchMsg

        SearchEffect (Search.BounceEffect delay msg) ->
            Bounce.delay delay msg
                |> Cmd.map SearchMsg

        StoreEffect (Store.GetAddressEffect { currency, address, toMsg }) ->
            Api.Request.Addresses.getAddress currency address (Just True)
                |> send apiKey effect (toMsg >> StoreMsg)


withAuthorization : String -> Api.Request a -> Api.Request a
withAuthorization apiKey request =
    Api.withHeader "Authorization" apiKey request


send : String -> Effect -> (a -> Msg) -> Api.Request a -> Cmd Msg
send apiKey effect toMsg =
    withAuthorization apiKey
        >> Api.sendAndAlsoReceiveHeaders BrowserGotResponseWithHeaders effect toMsg
