module Update.Pathfinder.ExportDialog exposing (update)

import Basics.Extra exposing (flip)
import Config.Update as Update
import Model exposing (Effect)
import Model.Dialog exposing (ExportConfig, ExportFormat, exportFormatToString)
import Msg.ExportDialog as ExportDialog exposing (Msg(..))
import Regex
import Util exposing (n)
import View.Locale as Locale


update : Update.Config -> ExportDialog.Msg -> ExportConfig Model.Msg -> ( ExportConfig Model.Msg, List Effect )
update uc msg model =
    case msg of
        UserClickedExport ->
            n { model | exporting = True }

        UserClickedAreaOption area ->
            n { model | area = area }

        UserClickedFormatOption format ->
            n
                { model
                    | fileFormat = format
                    , filename = normalizeFilename uc format model.filename
                }

        UserClickedKeepSelected ->
            n { model | keepSelectionHighlight = not model.keepSelectionHighlight }

        BrowserSentBBox _ ->
            -- handled in Update/Pathfinder.elm
            n model

        BrowserRenderedGraphForExport ->
            -- handled in Update/Pathfinder.elm
            n model

        BrowserSentExportGraphResult _ ->
            -- handled in Update/Pathfinder.elm
            n model

        UserInputsFilename filename ->
            n { model | filename = filename }

        UserLeavesFilename ->
            n
                { model
                    | filename = normalizeFilename uc model.fileFormat model.filename
                }


normalizeFilename : Update.Config -> ExportFormat -> String -> String
normalizeFilename uc format filename =
    let
        match =
            Regex.contains (Regex.fromString "\\.\\w{3}$" |> Maybe.withDefault Regex.never) filename
    in
    filename
        |> (if match then
                String.dropRight 4

            else
                identity
           )
        |> (\fn ->
                if String.isEmpty fn then
                    Locale.string uc.locale "Export-dialog-unknown-filename"

                else
                    fn
           )
        |> flip (++) ("." ++ exportFormatToString format)
