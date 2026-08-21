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
import Util.View exposing (loadingSpinner, onClickWithStop, testId)
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
            [ testId tid
            , css
                [ Css.cursor Css.pointer
                , Css.property "pointer-events" "bounding-box"
                ]
            , title (Locale.string vc.locale titl)
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

        title str =
            Locale.string vc.locale str
                |> HA.title
    in
    SettingsComponents.toolbarWithInstances
        (SettingsComponents.toolbarAttributes
            |> Rs.s_iconsDelete
                (iconsAttr "gs-toolbar-delete" "Delete" config.deleteDisabled UserClickedToolbarDeleteIcon)
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
                (iconsAttr "gs-toolbar-save" "save file" False (UserClickedSaveGraph Nothing))
            |> Rs.s_iconsExport
                (iconsAttr "gs-toolbar-export" "export graph" False (UserClickedExportGraph Nothing))
            |> Rs.s_iconsOpen
                (iconsAttr "gs-toolbar-open" "open" False UserClickedOpenGraph)
            |> Rs.s_iconsHorizontalAlign
                (iconsAttr "gs-toolbar-align-horizontal" "align horizontally" config.alignHorizontalDisabled UserClickedContextMenuAlignHorizontally)
        )
        (let
            ls =
                loadingSpinner vc
                    (\_ ->
                        [ Css.width <| Css.px 22
                        , Css.position Css.absolute
                        , Css.top (Css.px 1)
                        , Css.left (Css.px 1)
                        ]
                    )
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
                            (iconsAttr "gs-toolbar-redo" "Redo" config.redoDisabled UserClickedRedo)
                    )
                    { root = { state = Icons.IconsRedoStateActive } }
            }
        , iconsUndo =
            { variant =
                Icons.iconsUndoWithAttributes
                    (Icons.iconsUndoAttributes
                        |> Rs.s_root
                            (iconsAttr "gs-toolbar-undo" "Undo" config.undoDisabled UserClickedUndo)
                    )
                    { root = { state = Icons.IconsUndoStateActive } }
            }
        , root = { highlightVisible = False }
        }
