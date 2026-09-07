module View.Pathfinder.Toolbar exposing (Config, view)

import Config.View as View
import Css
import Html.Styled exposing (Html, div)
import Html.Styled.Attributes as HA exposing (css, id)
import Model.Pathfinder.Tools exposing (PointerTool(..), ToolbarHovercardType(..), toolbarHovercardTypeToId)
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), Msg(..))
import RecordSetter as Rs
import Theme.Colors
import Theme.Html.Icons as Icons
import Theme.Html.SettingsComponents as SettingsComponents
import Util.Pathfinder.Shortcuts as Shortcuts
import Util.View exposing (onClickWithStop, testId)
import Util.View.Loadingspinner as Loadingspinner
import View.Locale as Locale


type alias Config =
    { undoDisabled : Bool
    , redoDisabled : Bool
    , deleteDisabled : Bool
    , newDisabled : Bool
    , annotateDisabled : Bool
    , pointerTool : PointerTool
    , exportName : String
    , alignHorizontalDisabled : Bool
    , export : Bool
    }


view : View.Config -> Config -> Html Msg
view vc config =
    let
        iconsAttr tid titl disabled msg =
            iconsAttrWithHint tid titl Nothing disabled msg

        -- the tooltip carries the shortcut, e.g. "Save file (Ctrl+S)"
        iconsAttrWithHint tid titl shortcut disabled msg =
            [ testId tid
            , css
                [ Css.cursor Css.pointer
                , Css.property "pointer-events" "bounding-box"
                ]
            , Locale.string vc.locale titl
                ++ (shortcut
                        |> Maybe.map (Shortcuts.chord vc.isMac >> (\c -> " (" ++ c ++ ")"))
                        |> Maybe.withDefault ""
                   )
                |> HA.title
            ]
                ++ (if disabled then
                        [ css [ Css.opacity <| Css.num 0.3 ] ]

                    else
                        [ onClickWithStop msg ]
                   )

        highlightBackground pointer =
            [ Css.important <|
                Css.property "background-color" <|
                    if config.pointerTool == pointer then
                        Theme.Colors.toolbarHighlight

                    else
                        "transparent"
            , Css.borderRadius (Css.px 5)
            ]
    in
    SettingsComponents.toolbarWithInstances
        (SettingsComponents.toolbarAttributes
            |> Rs.s_iconsDelete
                (iconsAttrWithHint "gs-toolbar-delete" "Delete" (Just Shortcuts.deleteSelection) config.deleteDisabled UserClickedToolbarDeleteIcon)
            |> Rs.s_iconsAnnotate
                (id (toolbarHovercardTypeToId Annotation)
                    :: iconsAttr "gs-toolbar-annotate" "Annotate" config.annotateDisabled UserToggleAnnotationSettings
                )
            |> Rs.s_iconsNewFile
                (iconsAttr "gs-toolbar-new" "Restart" config.newDisabled UserClickedRestart)
            |> Rs.s_iconsSelectionTool
                (css (highlightBackground Select)
                    :: iconsAttr "gs-toolbar-select" "selection Tool" False UserClickedSelectionTool
                )
            |> Rs.s_iconsDisplayConfiguration
                (id (toolbarHovercardTypeToId Settings)
                    :: iconsAttr "gs-toolbar-display-settings" "Display settings" False (ChangedDisplaySettingsMsg UserClickedToggleDisplaySettings)
                )
            |> Rs.s_iconsCenterGraph
                (iconsAttr "gs-toolbar-center" "center graph" False UserClickedFitGraph)
            |> Rs.s_iconsSave
                (iconsAttrWithHint "gs-toolbar-save" "save file" (Just Shortcuts.save) False (UserClickedSaveGraph Nothing))
            |> Rs.s_iconsExport
                (iconsAttrWithHint "gs-toolbar-export" "export graph" (Just Shortcuts.export) False (UserClickedExportGraph Nothing))
            |> Rs.s_iconsOpen
                (iconsAttrWithHint "gs-toolbar-open" "open" (Just Shortcuts.open) False UserClickedOpenGraph)
            |> Rs.s_iconsHorizontalAlign
                (iconsAttr "gs-toolbar-align-horizontal" "align horizontally" config.alignHorizontalDisabled UserClickedContextMenuAlignHorizontally)
        )
        (let
            ls =
                Loadingspinner.html
                    [ css
                        [ Css.width <| Css.px 22
                        , Css.position Css.absolute
                        , Css.top (Css.px 1)
                        , Css.left (Css.px 1)
                        ]
                    ]
                    |> List.singleton
                    |> div
                        [ css
                            [ Css.width (Css.px 24)
                            , Css.height (Css.px 24)
                            , Css.position Css.relative
                            ]
                        ]
         in
         SettingsComponents.toolbarInstances
            |> Rs.s_iconsExport
                (if config.export then
                    Just ls

                 else
                    Nothing
                )
        )
        { iconsRedo =
            { variant =
                Icons.iconsRedoWithAttributes
                    (Icons.iconsRedoAttributes
                        |> Rs.s_root
                            (iconsAttrWithHint "gs-toolbar-redo" "Redo" (Just Shortcuts.redo) config.redoDisabled UserClickedRedo)
                    )
                    { root = { state = Icons.IconsRedoStateActive } }
            }
        , iconsUndo =
            { variant =
                Icons.iconsUndoWithAttributes
                    (Icons.iconsUndoAttributes
                        |> Rs.s_root
                            (iconsAttrWithHint "gs-toolbar-undo" "Undo" (Just Shortcuts.undo) config.undoDisabled UserClickedUndo)
                    )
                    { root = { state = Icons.IconsUndoStateActive } }
            }
        , root = { highlightVisible = False }
        }
