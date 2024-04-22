module Update.Pathfinder exposing (update, updateByRoute)

import Api.Data
import Config.Update as Update
import Dict
import Dict.Nonempty as NDict exposing (NonemptyDict)
import Effect exposing (and, n)
import Effect.Api as Api
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Init.Pathfinder
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Log
import Model.Direction exposing (Direction(..))
import Model.Graph exposing (Dragging(..))
import Model.Graph.Coords exposing (Coords)
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Locale exposing (State(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Tx as Tx
import Model.Search as Search
import Msg.Pathfinder as Msg exposing (Msg(..))
import Msg.Search as Search
import Plugin.Update as Plugin exposing (Plugins)
import RecordSetter exposing (..)
import RemoteData exposing (RemoteData(..))
import Result.Extra
import Route.Pathfinder as Route
import Tuple exposing (first, mapFirst, pair, second)
import Update.Graph exposing (draggingToClick)
import Update.Graph.History as History
import Update.Graph.Transform as Transform
import Update.Pathfinder.Network as Network
import Update.Search as Search
import Util.Pathfinder exposing (getAddress)
import Util.Pathfinder.History as History


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    model
        |> pushHistory msg
        |> updateByMsg plugins uc msg


resultLineToRoute : Search.ResultLine -> Route.Route
resultLineToRoute search =
    case search of
        Search.Address net address ->
            Route.Network net (Route.Address address)

        Search.Tx net h ->
            Route.Network net (Route.Tx h)

        Search.Block net b ->
            Route.Network net (Route.Block b)

        Search.Label s ->
            Route.Label s

        Search.Actor ( id, name ) ->
            Route.Actor id


updateByMsg : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
updateByMsg plugins uc msg model =
    case Log.truncate "msg" msg of
        PluginMsg _ ->
            -- handled in src/Update.elm
            n model

        NoOp ->
            n model

        BrowserGotActor id data ->
            n { model | actors = Dict.insert id data model.actors }

        BrowserGotNewAddress id data ->
            let
                ( m, e ) =
                    addAddress plugins id data model
            in
            ( selectAddress id m, e ++ fetchActorsForAddress data model.actors )

        BrowserGotRecentTx id direction data ->
            let
                getHash tx =
                    case tx of
                        Api.Data.AddressTxAddressTxUtxo t ->
                            t.txHash

                        Api.Data.AddressTxTxAccount t ->
                            t.txHash
            in
            ( model
            , data.addressTxs
                |> List.head
                |> Maybe.map getHash
                |> Maybe.map
                    (\txHash ->
                        BrowserGotTxForAddress id direction
                            |> Api.GetTxEffect
                                { currency = Id.network id
                                , txHash = txHash
                                , includeIo = True
                                , tokenTxId = Nothing
                                }
                            |> ApiEffect
                            |> List.singleton
                    )
                |> Maybe.withDefault []
            )

        BrowserGotTxForAddress id direction data ->
            browserGotTxForAddress plugins uc id direction data model

        SearchMsg m ->
            case m of
                Search.UserClicksResultLine ->
                    let
                        query =
                            Search.query model.search

                        selectedValue =
                            Search.selectedValue model.search

                        ( search, _ ) =
                            Search.update m model.search

                        m2 =
                            { model | search = search }
                    in
                    if String.isEmpty query then
                        n m2

                    else
                        case selectedValue of
                            Just value ->
                                value
                                    |> resultLineToRoute
                                    |> NavPushRouteEffect
                                    |> List.singleton
                                    |> Tuple.pair m2

                            Nothing ->
                                n m2

                _ ->
                    Search.update m model.search
                        |> Tuple.mapFirst (\s -> s_search s model)
                        |> Tuple.mapSecond (List.map Pathfinder.SearchEffect)

        UserClosedDetailsView ->
            n (closeDetailsView model)

        UserClickedToggleAddressDetailsTable ->
            n (toggleAddressDetailsTable model)

        UserClickedToggleTransactionDetailsTable ->
            n (toggleTransactionDetailsTable model)

        UserClickedRestart ->
            -- Handled upstream
            n model

        --n Init.Pathfinder.init
        -- TODO: Implement
        UserClickedUndo ->
            n model

        UserClickedRedo ->
            n model

        UserClickedHighlighter ->
            n model

        UserClickedImportFile ->
            n model

        UserClickedExportGraph ->
            n model

        UserClickedGraph dragging ->
            let
                click =
                    case dragging of
                        NoDragging ->
                            True

                        Dragging _ start current ->
                            draggingToClick start current

                        DraggingNode _ start current ->
                            draggingToClick start current
            in
            if click then
                ( model
                , Route.Root
                    |> NavPushRouteEffect
                    |> List.singleton
                )

            else
                n model

        UserReleasesMouseButton ->
            case model.dragging of
                NoDragging ->
                    n model

                Dragging _ start coords ->
                    n
                        { model
                            | dragging = NoDragging
                        }

                DraggingNode id start coords ->
                    n
                        { model
                            | network = Debug.todo "release node"
                            , dragging = NoDragging
                        }

        UserWheeledOnGraph x y z ->
            uc.size
                |> Maybe.map
                    (\size ->
                        { model
                            | transform =
                                Transform.wheel
                                    { width = size.width
                                    , height = size.height
                                    }
                                    x
                                    y
                                    z
                                    model.transform
                        }
                    )
                |> Maybe.withDefault model
                |> n

        UserPushesLeftMouseButtonOnGraph coords ->
            { model
                | dragging =
                    case ( model.dragging, model.transform.state ) of
                        ( NoDragging, Transform.Settled _ ) ->
                            Dragging model.transform coords coords

                        _ ->
                            NoDragging
            }
                |> n

        UserMovesMouseOnGraph coords ->
            case model.dragging of
                NoDragging ->
                    n model

                Dragging transform start _ ->
                    { model
                        | transform = Transform.update start coords transform
                        , dragging = Dragging transform start coords
                    }
                        |> n

                DraggingNode id start _ ->
                    let
                        vector =
                            Transform.vector start coords model.transform
                    in
                    { model
                        | dragging = DraggingNode id start coords
                    }
                        |> n

        AnimationFrameDeltaForTransform delta ->
            n
                { model
                    | transform = Transform.transition delta model.transform
                }

        UserClickedAddressExpandHandle id direction ->
            let
                ( nw, eff ) =
                    Network.expandAddress id direction model.network
            in
            ( { model | network = nw }
            , eff
            )

        UserClickedAddress id ->
            selectAddress id model
                |> n


updateByRoute : Plugins -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute plugins route model =
    forcePushHistory model
        |> updateByRoute_ plugins route


updateByRoute_ : Plugins -> Route.Route -> Model -> ( Model, List Effect )
updateByRoute_ plugins route model =
    case route |> Log.log "route" of
        Route.Root ->
            n model

        Route.Network network (Route.Address a) ->
            addressFromRoute plugins (Id.init network a) model

        _ ->
            n model


addressFromRoute : Plugins -> Id -> Model -> ( Model, List Effect )
addressFromRoute plugins id model =
    let
        ( nw, eff ) =
            Network.addressFromRoute plugins id model.network
    in
    ( { model | network = nw }
    , eff
    )


addAddress : Plugins -> Id -> Api.Data.Address -> Model -> ( Model, List Effect )
addAddress plugins id data model =
    { model
        | network = Network.updateAddress plugins id (s_data (Success data)) model.network
    }
        |> n


selectAddress : Id -> Model -> Model
selectAddress id =
    s_selection (SelectedAddress id)
        >> (setViewState <| s_detailsViewState (AddressDetails id { addressTableOpen = False, transactionsTableOpen = False }))


pushHistory : Msg -> Model -> Model
pushHistory msg model =
    if History.shallPushHistory msg model then
        forcePushHistory model

    else
        model


forcePushHistory : Model -> Model
forcePushHistory model =
    { model
        | history =
            makeHistoryEntry model
                |> History.push model.history
    }


makeHistoryEntry : Model -> Entry.Model
makeHistoryEntry model =
    { network = model.network
    }


undoRedo : (History.Model Entry.Model -> Entry.Model -> Maybe ( History.Model Entry.Model, Entry.Model )) -> Model -> ( Model, List Effect )
undoRedo fun model =
    makeHistoryEntry model
        |> fun model.history
        |> Maybe.map
            (\( history, entry ) ->
                { model
                    | history = history
                    , network = entry.network
                }
            )
        |> Maybe.withDefault model
        |> n


fetchActor : String -> Effect
fetchActor id =
    BrowserGotActor id |> Api.GetActorEffect { actorId = id } |> ApiEffect


fetchActorsForAddress : Api.Data.Address -> Dict.Dict String Api.Data.Actor -> List Effect
fetchActorsForAddress d existing =
    d.actors
        |> Maybe.map (List.filter (\l -> not (Dict.member l.id existing)))
        |> Maybe.map (List.map (.id >> fetchActor))
        |> Maybe.withDefault []


browserGotTxForAddress : Plugins -> Update.Config -> Id -> Direction -> Api.Data.Tx -> Model -> ( Model, List Effect )
browserGotTxForAddress plugins uc id direction data model =
    getAddress model.network.addresses id
        |> Result.map (\{ x, y } -> Coords x y)
        |> Result.andThen (Tx.fromData data direction)
        |> Result.map
            (\tx ->
                let
                    ( nw, eff ) =
                        Network.addAddressAt plugins id direction firstAddress model.network
                            |> and (Network.insertTx tx)

                    getBiggest io =
                        NDict.toList io
                            |> List.sortBy (second >> .value)
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault (NDict.head io)
                            |> first

                    firstAddress =
                        case tx.type_ of
                            Tx.Utxo t ->
                                case direction of
                                    Incoming ->
                                        getBiggest t.inputs

                                    Outgoing ->
                                        getBiggest t.outputs

                            Tx.Account t ->
                                case direction of
                                    Incoming ->
                                        t.from

                                    Outgoing ->
                                        t.to
                in
                ( { model | network = nw }
                , eff
                )
            )
        |> Result.Extra.extract
            (ErrorEffect >> List.singleton >> pair model)
