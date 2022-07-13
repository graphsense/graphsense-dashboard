module Setup exposing (start)

import Api exposing (Request)
import Api.Request.Addresses
import Api.Request.Entities
import Api.Request.General
import Api.Request.Txs
import Dict
import Effect.Graph as Graph
import Effect.Search as Search
import Init exposing (init)
import Init.Locale as Locale
import Model exposing (Effect(..), Flags, Model, Msg(..))
import ProgramTest exposing (ProgramTest)
import Result.Extra
import Setup.Graph as Graph
import Setup.Locale as Locale
import Setup.Search as Search
import SimulatedEffect.Cmd
import SimulatedEffect.Http as Http
import Theme.Theme as Theme
import Tuple exposing (first)
import Update exposing (update)
import Util.Debug
import View exposing (view)


start : String -> ProgramTest (Model ()) Msg (List Effect)
start locale =
    let
        uc =
            { defaultColor = Theme.default.graph.defaultColor
            , colorScheme = Theme.default.graph.colorScheme
            }

        plugins =
            Dict.empty

        initialPath =
            "/"

        flags =
            { locale = locale
            , height = 800
            , width = 1200
            , now = 0
            }
    in
    ProgramTest.createApplication
        { onUrlChange = BrowserChangedUrl
        , onUrlRequest = UserRequestsUrl
        , init = init plugins
        , update = Util.Debug.addDebugToUpdate (update plugins uc)
        , view =
            \model ->
                let
                    _ =
                        Debug.log "VIEW" model
                in
                view
                    plugins
                    { theme = Theme.default
                    , locale = model.locale
                    }
                    model
        }
        |> ProgramTest.withBaseUrl ("http://foo.bar" ++ initialPath)
        |> ProgramTest.withSimulatedEffects simulateEffects
        |> ProgramTest.start flags


simulateEffects : List Effect -> ProgramTest.SimulatedEffect Msg
simulateEffects effects =
    List.map simulateEffect effects
        |> SimulatedEffect.Cmd.batch


simulateEffect : Effect -> ProgramTest.SimulatedEffect Msg
simulateEffect effect =
    let
        apiKey =
            ""
    in
    case effect of
        NavLoadEffect _ ->
            SimulatedEffect.Cmd.none

        NavPushUrlEffect _ ->
            SimulatedEffect.Cmd.none

        GetElementEffect _ ->
            SimulatedEffect.Cmd.none

        GetStatisticsEffect ->
            Api.Request.General.getStatistics
                |> Api.effect BrowserGotStatistics

        LocaleEffect eff ->
            Locale.simulateEffects eff
                |> SimulatedEffect.Cmd.map LocaleMsg

        SearchEffect (Search.SearchEffect { query, currency, limit, toMsg }) ->
            Api.Request.General.search query currency limit
                |> Api.effect (Result.Extra.unpack (\_ -> NoOp) (toMsg >> SearchMsg))

        SearchEffect eff ->
            Search.simulateEffects eff
                |> SimulatedEffect.Cmd.map SearchMsg

        GraphEffect eff ->
            case eff of
                Graph.NavPushRouteEffect route ->
                    SimulatedEffect.Cmd.none

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

                _ ->
                    Graph.simulateEffects eff
                        |> SimulatedEffect.Cmd.map GraphMsg

        PortsConsoleEffect _ ->
            SimulatedEffect.Cmd.none


send : String -> Effect -> (a -> Msg) -> Request a -> ProgramTest.SimulatedEffect Msg
send _ _ toMsg =
    Api.effect (Result.Extra.unpack (\_ -> NoOp) toMsg)
