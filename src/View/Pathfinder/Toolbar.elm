module View.Pathfinder.Toolbar exposing (Config, view)

import Config.View as View
import Css
import Html.Styled exposing (Html)
import Html.Styled.Attributes as HA exposing (css, id)
import Model.Pathfinder.Tools exposing (PointerTool(..), ToolbarHovercardType(..), toolbarHovercardTypeToId)
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), Msg(..))
import RecordSetter as Rs
import Theme.Colors
import Theme.Html.Icons as Icons
import Theme.Html.SettingsComponents as SettingsComponents
import Util.View exposing (onClickWithStop)
import View.Locale as Locale


type alias Config =
    { undoDisabled : Bool
    , redoDisabled : Bool
    , deleteDisabled : Bool
    , newDisabled : Bool
    , annotateDisabled : Bool
    , pointerTool : PointerTool
    , exportName : String
    }


view : View.Config -> Config -> Html Msg
view vc config =
    let
        iconsAttr =
            [ css
                [ Css.cursor Css.pointer
                , Css.property "pointer-events" "bounding-box"
                ]
            ]

        highlightBackground pointer =
            [ Css.important <|
                Css.property "background-color" <|
                    if config.pointerTool == pointer then
                        Theme.Colors.brandLight

                    else
                        "transparent"
            ]

        title str =
            Locale.string vc.locale str
                |> HA.title
    in
    SettingsComponents.toolbarWithInstances
        (SettingsComponents.toolbarAttributes
            |> Rs.s_iconsDelete
                (onClickWithStop UserClickedToolbarDeleteIcon
                    :: title (Locale.string vc.locale "Delete")
                    :: (iconsAttr
                            ++ (if config.deleteDisabled then
                                    [ css [ Css.opacity <| Css.num 0.3 ] ]

                                else
                                    []
                               )
                       )
                )
            |> Rs.s_iconsAnnotate
                (onClickWithStop UserToggleAnnotationSettings
                    :: id (toolbarHovercardTypeToId Annotation)
                    :: title (Locale.string vc.locale "Annotate")
                    :: (iconsAttr
                            ++ (if config.annotateDisabled then
                                    [ css [ Css.opacity <| Css.num 0.3 ] ]

                                else
                                    []
                               )
                       )
                )
            |> Rs.s_iconsNewFile
                (onClickWithStop UserClickedRestart
                    :: title (Locale.string vc.locale "Restart")
                    :: (iconsAttr
                            ++ (if config.newDisabled then
                                    [ css [ Css.opacity <| Css.num 0.3 ] ]

                                else
                                    []
                               )
                       )
                )
            |> Rs.s_iconsSelectionTool
                (onClickWithStop UserClickedSelectionTool
                    :: title (Locale.string vc.locale "Selection tool")
                    :: css (highlightBackground Select)
                    :: iconsAttr
                )
            |> Rs.s_iconsDisplayConfiguration
                (id (toolbarHovercardTypeToId Settings)
                    :: title (Locale.string vc.locale "Display settings")
                    :: onClickWithStop (ChangedDisplaySettingsMsg UserClickedToggleDisplaySettings)
                    :: iconsAttr
                )
            |> Rs.s_iconsCenterGraph
                (onClickWithStop UserClickedFitGraph
                    :: title (Locale.string vc.locale "Center graph")
                    :: iconsAttr
                )
            |> Rs.s_iconsSave
                (onClickWithStop (UserClickedSaveGraph Nothing)
                    :: title (Locale.string vc.locale "Save")
                    :: iconsAttr
                )
            |> Rs.s_iconsScrennshot
                (onClickWithStop (UserClickedExportGraphAsImage config.exportName)
                    :: title (Locale.string vc.locale "Screenshot")
                    :: iconsAttr
                )
            |> Rs.s_iconsOpen
                (onClickWithStop UserClickedOpenGraph
                    :: title (Locale.string vc.locale "Open")
                    :: iconsAttr
                )
        )
        SettingsComponents.toolbarInstances
        { iconsRedo =
            { variant =
                if config.redoDisabled then
                    Icons.iconsRedoStateDisabledWithAttributes
                        (Icons.iconsRedoStateDisabledAttributes
                            |> Rs.s_stateDisabled
                                [ title (Locale.string vc.locale "Redo")
                                ]
                        )
                        {}

                else
                    Icons.iconsRedoStateActiveWithAttributes
                        (Icons.iconsRedoStateActiveAttributes
                            |> Rs.s_stateActive
                                (onClickWithStop UserClickedRedo
                                    :: title (Locale.string vc.locale "Redo")
                                    :: iconsAttr
                                )
                        )
                        {}
            }
        , iconsUndo =
            { variant =
                if config.undoDisabled then
                    Icons.iconsUndoStateDisabledWithAttributes
                        (Icons.iconsUndoStateDisabledAttributes
                            |> Rs.s_stateDisabled
                                [ title (Locale.string vc.locale "Undo")
                                ]
                        )
                        {}

                else
                    Icons.iconsUndoStateActiveWithAttributes
                        (Icons.iconsUndoStateActiveAttributes
                            |> Rs.s_stateActive
                                (onClickWithStop UserClickedUndo
                                    :: title (Locale.string vc.locale "Undo")
                                    :: iconsAttr
                                )
                        )
                        {}
            }
        , toolbar = { highlightVisible = False }
        }
