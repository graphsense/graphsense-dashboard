module Effect exposing (n, perform)

import Api
import Api.Request.Addresses
import Api.Request.Entities
import Api.Request.General
import Api.Request.Tags
import Bounce
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Search as Search
import Http
import Model exposing (Auth(..), Effect(..), Msg(..))
import Msg.Graph as Graph
import Msg.Search as Search
import Plugin exposing (Plugins)
import Plugin.Effect
import Ports
import Route
import Task


n : m -> ( m, List eff )
n m =
    ( m, [] )


perform : Plugins -> Nav.Key -> String -> Effect -> Cmd Msg
perform plugins key apiKey effect =
    case effect of
        NavLoadEffect url ->
            Nav.load url

        NavPushUrlEffect url ->
            Nav.pushUrl key url

        GetStatisticsEffect ->
            Api.Request.General.getStatistics
                |> Api.send BrowserGotStatistics

        GetConceptsEffect taxonomy msg ->
            Api.Request.Tags.listConcepts taxonomy
                |> send apiKey effect msg

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

                Graph.GetEntityNeighborsEffect { currency, entity, isOutgoing, pagesize, onlyIds, includeLabels, nextpage, toMsg } ->
                    let
                        direction =
                            case isOutgoing of
                                True ->
                                    Api.Request.Entities.DirectionOut

                                False ->
                                    Api.Request.Entities.DirectionIn
                    in
                    Api.Request.Entities.listEntityNeighbors currency entity direction onlyIds (Just includeLabels) nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressNeighborsEffect { currency, address, isOutgoing, pagesize, includeLabels, nextpage, toMsg } ->
                    let
                        direction =
                            case isOutgoing of
                                True ->
                                    Api.Request.Addresses.DirectionOut

                                False ->
                                    Api.Request.Addresses.DirectionIn
                    in
                    Api.Request.Addresses.listAddressNeighbors currency address direction (Just includeLabels) nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressEffect { currency, address, toMsg } ->
                    Api.Request.Addresses.getAddress currency address
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityEffect { currency, entity, toMsg } ->
                    Api.Request.Entities.getEntity currency entity
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityForAddressEffect { currency, address, toMsg } ->
                    Api.Request.Addresses.getAddressEntity currency address
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressTxsEffect { currency, address, pagesize, nextpage, toMsg } ->
                    Api.Request.Addresses.listAddressTxs currency address nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressTagsEffect { currency, address, pagesize, nextpage, toMsg } ->
                    Api.Request.Addresses.listTagsByAddress currency address nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityAddressTagsEffect { currency, entity, pagesize, nextpage, toMsg } ->
                    Api.Request.Entities.listAddressTagsByEntity currency entity nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityAddressesEffect { currency, entity, pagesize, nextpage, toMsg } ->
                    Api.Request.Entities.listEntityAddresses currency entity nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityTxsEffect { currency, entity, pagesize, nextpage, toMsg } ->
                    Api.Request.Entities.listEntityTxs currency entity nextpage (Just pagesize)
                        |> send apiKey effect (toMsg >> GraphMsg)

                Graph.GetSvgElementEffect ->
                    Graph.perform eff
                        |> Cmd.map GraphMsg

                Graph.GetBrowserElementEffect ->
                    Graph.perform eff
                        |> Cmd.map GraphMsg

                Graph.InternalGraphAddedAddressesEffect ids ->
                    Task.succeed ids
                        |> Task.perform (Graph.InternalGraphAddedAddresses >> GraphMsg)

                Graph.PluginEffect _ ->
                    Graph.perform eff
                        |> Cmd.map GraphMsg

                Graph.TagSearchEffect e ->
                    handleSearchEffect apiKey
                        Nothing
                        (Graph.TagSearchMsg >> GraphMsg)
                        (Graph.TagSearchEffect >> GraphEffect)
                        e

                Graph.CmdEffect cmd ->
                    cmd
                        |> Cmd.map GraphMsg

        SearchEffect e ->
            handleSearchEffect apiKey (Just plugins) SearchMsg SearchEffect e

        PortsConsoleEffect msg ->
            Ports.console msg

        PluginEffect ( pid, cmd ) ->
            cmd
                |> Cmd.map (PluginMsg pid)

        CmdEffect cmd ->
            cmd


handleSearchEffect : String -> Maybe Plugins -> (Search.Msg -> Msg) -> (Search.Effect -> Effect) -> Search.Effect -> Cmd Msg
handleSearchEffect apiKey plugins tag tagEffect effect =
    case effect of
        Search.SearchEffect { query, currency, limit, toMsg } ->
            (Api.Request.General.search query currency limit
                |> Api.withTracker "search"
                |> send apiKey (tagEffect effect) (toMsg >> tag)
            )
                :: (plugins
                        |> Maybe.map (\p -> Plugin.Effect.search p query)
                        |> Maybe.withDefault []
                   )
                |> Cmd.batch

        Search.CancelEffect ->
            Http.cancel "search"
                |> Cmd.map tag

        Search.BounceEffect delay msg ->
            Bounce.delay delay msg
                |> Cmd.map tag


withAuthorization : String -> Api.Request a -> Api.Request a
withAuthorization apiKey request =
    Api.withHeader "Authorization" apiKey request


send : String -> Effect -> (a -> Msg) -> Api.Request a -> Cmd Msg
send apiKey effect toMsg =
    withAuthorization apiKey
        >> Api.sendAndAlsoReceiveHeaders BrowserGotResponseWithHeaders effect toMsg
