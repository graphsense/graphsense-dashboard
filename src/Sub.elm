module Sub exposing (subscriptions)

import Browser.Events
import Browser.Navigation as Nav
import Hovercard
import Model exposing (Model, Msg(..))
import Msg.ExportDialog as ExportDialog
import Msg.Graph as MsgGraph
import Plugin.Sub as Plugin
import Ports
import Sub.Graph as Graph
import Sub.Locale as Locale
import Sub.Pathfinder as Pathfinder
import Time


subscriptions : Model Nav.Key -> Sub Msg
subscriptions model =
    [ Locale.subscriptions model.config.locale
        |> Sub.map LocaleMsg
    , case model.page of
        Model.Graph ->
            Graph.subscriptions model.graph
                |> Sub.map GraphMsg

        Model.Pathfinder ->
            Pathfinder.subscriptions model.pathfinder
                |> Sub.map PathfinderMsg

        _ ->
            Sub.none
    , Browser.Events.onResize
        BrowserChangedWindowSize
    , case model.user.auth of
        Model.Authorized auth ->
            case auth.requestLimit of
                Model.Limited { remaining, reset } ->
                    if reset > 0 && remaining < Model.showResetCounterAtRemaining then
                        Time.every 1000 TimeUpdateReset

                    else
                        Sub.none

                _ ->
                    Sub.none

        _ ->
            Sub.none
    , model.user.hovercard
        |> Maybe.map (Hovercard.subscriptions >> Sub.map UserHovercardMsg)
        |> Maybe.withDefault Sub.none
    , model.tooltip
        |> Maybe.map (.hovercard >> Hovercard.subscriptions >> Sub.map HovercardMsg)
        |> Maybe.withDefault Sub.none
    , Ports.sendBBox (ExportDialog.BrowserSentBBox >> ExportDialogMsg)
    , Ports.renderedImageForExport (\_ -> ExportDialog.BrowserRenderedGraphForExport |> ExportDialogMsg)
    , Ports.exportGraphResult (ExportDialog.BrowserSentExportGraphResult >> ExportDialogMsg)
    , Ports.deserialized (MsgGraph.PortDeserializedGS >> GraphMsg)
    , Ports.uncaughtError BrowserGotUncaughtError
    , Plugin.subscriptions Ports.pluginsIn model.plugins
        |> Sub.map PluginMsg
    ]
        |> Sub.batch
