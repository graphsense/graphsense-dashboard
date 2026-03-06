module Update.Pathfinder.ExportDialog exposing (update)

import Basics.Extra exposing (flip)
import Config.Update as Update
import Model exposing (Effect)
import Model.Dialog exposing (ExportConfig, ExportFormat, exportFormatToString)
import Msg.ExportDialog as ExportDialog exposing (Msg(..))
import Regex
import Util exposing (n)


update : Update.Config -> ExportDialog.Msg -> ExportConfig Model.Msg -> ( ExportConfig Model.Msg, List Effect )
update _ msg model =
    case msg of
        UserClickedExport ->
            n model

        UserClickedAreaOption area ->
            n { model | area = area }

        UserClickedFormatOption format ->
            n
                { model
                    | fileFormat = format
                    , filename = normalizeFilename format model.filename
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
                    | filename = normalizeFilename model.fileFormat model.filename
                }


normalizeFilename : ExportFormat -> String -> String
normalizeFilename format filename =
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
        |> flip (++) ("." ++ exportFormatToString format)
