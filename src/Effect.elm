module Effect exposing (n, perform)

import Api
import Api.Request.Addresses
import Api.Request.Entities
import Api.Request.General
import Bounce
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Search as Search
import Http
import Model exposing (Auth(..), Effect(..), Msg(..))
import Msg.Graph as Graph
import Plugin exposing (Plugins)
import Ports
import Route
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
            case eff of
                Graph.NavPushRouteEffect route ->
                    Route.graphRoute route
                        |> Route.toUrl
                        |> Nav.pushUrl key

                Graph.GetEntityNeighborsEffect { currency, entity, isOutgoing, pagesize, onlyIds, toMsg } ->
                    let
                        direction =
                            case isOutgoing of
                                True ->
                                    Api.Request.Entities.DirectionOut

                                False ->
                                    Api.Request.Entities.DirectionIn
                    in
                    Api.Request.Entities.listEntityNeighbors currency entity direction onlyIds Nothing Nothing (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressNeighborsEffect { currency, address, isOutgoing, pagesize, toMsg } ->
                    let
                        direction =
                            case isOutgoing of
                                True ->
                                    Api.Request.Addresses.DirectionOut

                                False ->
                                    Api.Request.Addresses.DirectionIn
                    in
                    Api.Request.Addresses.listAddressNeighbors currency address direction Nothing Nothing (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressEffect { currency, address, toMsg } ->
                    Api.Request.Addresses.getAddress currency address (Just True)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityEffect { currency, entity, toMsg } ->
                    Api.Request.Entities.getEntity currency entity (Just True)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityForAddressEffect { currency, address, toMsg } ->
                    Api.Request.Addresses.getAddressEntity currency address (Just True)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressTxsEffect { currency, address, pagesize, nextpage, toMsg } ->
                    Api.Request.Addresses.listAddressTxs currency address nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressTagsEffect { currency, address, pagesize, nextpage, toMsg } ->
                    Api.Request.Addresses.listTagsByAddress currency address nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityAddressesEffect { currency, entity, pagesize, nextpage, toMsg } ->
                    Api.Request.Entities.listEntityAddresses currency entity nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                _ ->
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

        PortsConsoleEffect msg ->
            Ports.console msg


withAuthorization : String -> Api.Request a -> Api.Request a
withAuthorization apiKey request =
    Api.withHeader "Authorization" apiKey request


send : String -> Effect -> (a -> Msg) -> Api.Request a -> Cmd Msg
send apiKey effect toMsg =
    withAuthorization apiKey
        >> Api.sendAndAlsoReceiveHeaders BrowserGotResponseWithHeaders effect toMsg
