module Effect exposing (n, perform)

import Api
import Api.Data
import Api.Request.Addresses
import Api.Request.Blocks
import Api.Request.Entities
import Api.Request.General
import Api.Request.MyBulk
import Api.Request.Tags
import Api.Request.Txs
import Bounce
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Effect.Graph as Graph
import Effect.Locale as Locale
import Effect.Search as Search
import Http
import Json.Decode
import Json.Encode
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


perform : Plugins -> Nav.Key -> Maybe String -> String -> Effect -> Cmd Msg
perform plugins key statusbarToken apiKey effect =
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
                |> send statusbarToken apiKey effect msg

        GetElementEffect { id, msg } ->
            Dom.getElement id
                |> Task.attempt msg

        LocaleEffect eff ->
            Locale.perform eff
                |> Cmd.map LocaleMsg

        LogoutEffect ->
            Http.riskyRequest
                { method = "GET"
                , headers = [ Http.header "Authorization" apiKey ]
                , url = Api.baseUrl ++ "/search?logout"
                , body = Http.emptyBody
                , expect = Http.expectWhatever BrowserGotLoggedOut
                , timeout = Nothing
                , tracker = Nothing
                }

        GraphEffect eff ->
            case eff of
                Graph.NavPushRouteEffect route ->
                    Route.graphRoute route
                        |> Route.toUrl
                        |> Nav.pushUrl key

                Graph.GetEntityNeighborsEffect { currency, entity, isOutgoing, pagesize, onlyIds, includeLabels, nextpage, toMsg } ->
                    let
                        direction =
                            isOutgoingToDirection isOutgoing
                    in
                    Api.Request.Entities.listEntityNeighbors currency entity direction onlyIds (Just includeLabels) nextpage (Just pagesize)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

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
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressEffect { currency, address, toMsg } ->
                    Api.Request.Addresses.getAddress currency address
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityEffect { currency, entity, toMsg } ->
                    Api.Request.Entities.getEntity currency entity
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetBlockEffect { currency, height, toMsg } ->
                    Api.Request.Blocks.getBlock currency height
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityForAddressEffect { currency, address, toMsg } ->
                    Api.Request.Addresses.getAddressEntity currency address
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressTxsEffect { currency, address, pagesize, nextpage, toMsg } ->
                    Api.Request.Addresses.listAddressTxs currency address nextpage (Just pagesize)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddresslinkTxsEffect { currency, source, target, pagesize, nextpage, toMsg } ->
                    Api.Request.Addresses.listAddressLinks currency source target nextpage (Just pagesize)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntitylinkTxsEffect { currency, source, target, pagesize, nextpage, toMsg } ->
                    Api.Request.Entities.listEntityLinks currency source target nextpage (Just pagesize)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetAddressTagsEffect { currency, address, pagesize, nextpage, toMsg } ->
                    Api.Request.Addresses.listTagsByAddress currency address nextpage (Just pagesize)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityAddressTagsEffect { currency, entity, pagesize, nextpage, toMsg } ->
                    Api.Request.Entities.listAddressTagsByEntity currency entity nextpage (Just pagesize)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityAddressesEffect { currency, entity, pagesize, nextpage, toMsg } ->
                    Api.Request.Entities.listEntityAddresses currency entity nextpage (Just pagesize)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetEntityTxsEffect { currency, entity, pagesize, nextpage, toMsg } ->
                    Api.Request.Entities.listEntityTxs currency entity nextpage (Just pagesize)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetBlockTxsEffect { currency, block, toMsg } ->
                    Api.Request.Blocks.listBlockTxs currency block
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetTxEffect { currency, txHash, toMsg } ->
                    Api.Request.Txs.getTx currency txHash (Just False)
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetTxUtxoAddressesEffect { currency, txHash, isOutgoing, toMsg } ->
                    let
                        io =
                            if isOutgoing then
                                Api.Request.Txs.IoOutputs

                            else
                                Api.Request.Txs.IoInputs
                    in
                    Api.Request.Txs.getTxIo currency txHash io
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.SearchEntityNeighborsEffect e ->
                    let
                        direction =
                            isOutgoingToDirection e.isOutgoing
                    in
                    Api.Request.Entities.searchEntityNeighbors e.currency e.entity direction e.key e.value e.depth (Just e.breadth) (Just e.maxAddresses)
                        |> send statusbarToken apiKey effect (e.toMsg >> GraphMsg)

                Graph.ListAddressTagsEffect { label, nextpage, pagesize, toMsg } ->
                    Api.Request.Tags.listAddressTags label nextpage pagesize
                        |> send statusbarToken apiKey effect (toMsg >> GraphMsg)

                Graph.GetSvgElementEffect ->
                    Graph.perform eff
                        |> Cmd.map GraphMsg

                Graph.GetBrowserElementEffect ->
                    Graph.perform eff
                        |> Cmd.map GraphMsg

                Graph.BulkGetAddressEffect e ->
                    listWithMaybes Api.Data.addressDecoder
                        |> Api.Request.MyBulk.bulkJson
                            e.currency
                            Api.Request.MyBulk.OperationGetAddress
                            (Json.Encode.object
                                [ ( "address", Json.Encode.list Json.Encode.string e.addresses )
                                ]
                            )
                        |> send statusbarToken apiKey effect (e.toMsg >> GraphMsg)

                Graph.BulkGetAddressTagsEffect e ->
                    listWithMaybes Api.Data.addressTagDecoder
                        |> Api.Request.MyBulk.bulkJson
                            e.currency
                            Api.Request.MyBulk.OperationListTagsByAddress
                            (Json.Encode.object
                                [ ( "address", Json.Encode.list Json.Encode.string e.addresses )
                                ]
                            )
                        |> send statusbarToken apiKey effect (e.toMsg >> GraphMsg)

                Graph.BulkGetEntityEffect e ->
                    listWithMaybes Api.Data.entityDecoder
                        |> Api.Request.MyBulk.bulkJson
                            e.currency
                            Api.Request.MyBulk.OperationGetEntity
                            (Json.Encode.object
                                [ ( "entity", Json.Encode.list Json.Encode.int e.entities )
                                ]
                            )
                        |> send statusbarToken apiKey effect (e.toMsg >> GraphMsg)

                Graph.BulkGetAddressEntityEffect e ->
                    listWithMaybes Api.Data.entityDecoder
                        |> Api.Request.MyBulk.bulkJson
                            e.currency
                            Api.Request.MyBulk.OperationGetAddressEntity
                            (Json.Encode.object
                                [ ( "address", Json.Encode.list Json.Encode.string e.addresses )
                                ]
                            )
                        |> send statusbarToken apiKey effect (e.toMsg >> GraphMsg)

                Graph.BulkGetEntityNeighborsEffect e ->
                    listWithMaybes
                        (Json.Decode.field "_request_entity" Json.Decode.int
                            |> Json.Decode.andThen
                                (\requestEntity ->
                                    Json.Decode.map
                                        (\entity -> ( requestEntity, entity ))
                                        Api.Data.neighborEntityDecoder
                                )
                        )
                        |> Api.Request.MyBulk.bulkJson
                            e.currency
                            Api.Request.MyBulk.OperationListEntityNeighbors
                            (Json.Encode.object
                                [ ( "entity", Json.Encode.list Json.Encode.int e.entities )
                                , ( "direction"
                                  , Json.Encode.string <|
                                        Api.Request.Entities.stringFromDirection <|
                                            if e.isOutgoing then
                                                Api.Request.Entities.DirectionOut

                                            else
                                                Api.Request.Entities.DirectionIn
                                  )
                                , ( "only_ids", Json.Encode.list Json.Encode.int e.entities )
                                ]
                            )
                        |> send statusbarToken apiKey effect (e.toMsg >> GraphMsg)

                Graph.InternalGraphAddedAddressesEffect ids ->
                    Task.succeed ids
                        |> Task.perform (Graph.InternalGraphAddedAddresses >> GraphMsg)

                Graph.InternalGraphAddedEntitiesEffect ids ->
                    Task.succeed ids
                        |> Task.perform (Graph.InternalGraphAddedEntities >> GraphMsg)

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
                |> send Nothing apiKey (tagEffect effect) (toMsg >> tag)
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


send : Maybe String -> String -> Effect -> (a -> Msg) -> Api.Request a -> Cmd Msg
send statusbarToken apiKey effect toMsg =
    withAuthorization apiKey
        >> Api.sendAndAlsoReceiveHeaders (BrowserGotResponseWithHeaders statusbarToken) effect toMsg


isOutgoingToDirection : Bool -> Api.Request.Entities.Direction
isOutgoingToDirection isOutgoing =
    case isOutgoing of
        True ->
            Api.Request.Entities.DirectionOut

        False ->
            Api.Request.Entities.DirectionIn


listWithMaybes : Json.Decode.Decoder a -> Json.Decode.Decoder (List a)
listWithMaybes decoder =
    Json.Decode.list (Json.Decode.maybe decoder)
        |> Json.Decode.map (List.filterMap identity)
