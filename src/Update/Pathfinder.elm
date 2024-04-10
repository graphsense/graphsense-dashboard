module Update.Pathfinder exposing (update)

import Config.Update as Update
import Effect exposing (n)
import Effect.Pathfinder exposing (Effect(..))
import Log
import Model.Graph exposing (Dragging(..))
import Model.Graph.History as History
import Model.Graph.Transform as Transform
import Model.Locale exposing (State(..))
import Model.Pathfinder exposing (..)
import Model.Pathfinder.History.Entry as Entry
import Msg.Pathfinder as Msg exposing (Msg(..))
import Plugin.Update as Plugin exposing (Plugins)
import Route.Pathfinder as Route
import Update.Graph exposing (draggingToClick)
import Update.Graph.History as History
import Update.Graph.Transform as Transform
import Util.Pathfinder.History as History


update : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
update plugins uc msg model =
    model
        |> pushHistory msg
        |> updateByMsg plugins uc msg


updateByMsg : Plugins -> Update.Config -> Msg -> Model -> ( Model, List Effect )
updateByMsg plugins uc msg model =
    case Log.truncate "msg" msg of
        PluginMsg _ ->
            -- handled in src/Update.elm
            n model

        NoOp ->
            n model

        -- TODO: Implement
        SearchMsg _ ->
            n model

        UserClosedPropertyBox ->
            n model

        UserClickedRestart ->
            n model

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
                            model.dragging
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
