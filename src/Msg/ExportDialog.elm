module Msg.ExportDialog exposing (Msg(..))

import Model.Dialog exposing (ExportArea, ExportFormat)
import Model.Graph.Coords exposing (BBox)


type Msg
    = UserClickedExport
    | UserClickedAreaOption ExportArea
    | UserClickedKeepSelected
    | UserClickedFormatOption ExportFormat
    | BrowserSentBBox (Maybe BBox)
    | BrowserRenderedGraphForExport
    | BrowserSentExportGraphResult (Maybe String)
    | UserInputsFilename String
    | UserLeavesFilename
    | UserClickedTransparentBackground
