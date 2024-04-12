module Update.Pathfinder exposing (update, updateByRoute)

import Api.Data
import Config.Update as Update
import Effect exposing (n)
import Effect.Api as Api
import Effect.Pathfinder as Pathfinder exposing (Effect(..))
import Init.Pathfinder
import Init.Pathfinder.Id as Id
import Init.Pathfinder.Network as Network
import Log
import Model.Graph exposing (Dragging(..))
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Locale exposing (State(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.History.Entry as Entry
import Model.Pathfinder.Id exposing (Id)
import Model.Search as Search
import Msg.Pathfinder as Msg exposing (Msg(..))
import Msg.Search as Search
import Plugin.Update as Plugin exposing (Plugins)
import RecordSetter exposing (..)
import Route.Pathfinder as Route
import Tuple
import Update.Graph exposing (draggingToClick)
import Update.Graph.History as History
import Update.Graph.Transform as Transform
import Update.Pathfinder.Network as Network
import Update.Search as Search
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

        BrowserGotAddress id data ->
            let
                ( m, e ) =
                    addAddress plugins
                        id
                        data
                        model
            in
            ( selectAddress data m, e )

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
            ( model, [ Api.GetAddressEffect { currency = network, address = a } (BrowserGotAddress (Id.init network a)) |> ApiEffect ] )

        _ ->
            n model


addAddress : Plugins -> Id -> Api.Data.Address -> Model -> ( Model, List Effect )
addAddress plugins id data model =
    let
        ( nw, eff ) =
            Network.addAddress plugins id data model.network
    in
    ( { model | network = nw }
    , eff
    )


selectAddress : Api.Data.Address -> Model -> Model
selectAddress a =
    let
        id =
            Id.init a.currency a.address
    in
    setSelection (SelectedAddress id)
        >> (setViewState <| setDetailsViewState (AddressDetails id { addressTableOpen = False, transactionsTableOpen = False }))


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
