module Model.Pathfinder.Tools exposing (PointerTool(..), ToolbarHovercardModel, ToolbarHovercardType(..), toolbarHovercardTypeToId)

import Hovercard


type PointerTool
    = Drag
    | Select


type alias ToolbarHovercardModel =
    ( ToolbarHovercardType, Hovercard.Model )


type ToolbarHovercardType
    = Settings
    | Annotation


toolbarHovercardTypeToId : ToolbarHovercardType -> String
toolbarHovercardTypeToId thct =
    case thct of
        Settings ->
            "toolbar-display-settings"

        Annotation ->
            "toolbar-annotation-settings"
