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
        iconsAttr titl disabled msg =
            [ css
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
    SettingsComponents.toolbarWithAttributes
        (SettingsComponents.toolbarAttributes
            |> Rs.s_iconsDelete
                (iconsAttr "Delete" config.deleteDisabled UserClickedToolbarDeleteIcon)
            |> Rs.s_iconsAnnotate
                (id (toolbarHovercardTypeToId Annotation)
                    :: iconsAttr "Annotate" config.annotateDisabled UserToggleAnnotationSettings
                )
            |> Rs.s_iconsNewFile
                (iconsAttr "Restart" config.newDisabled UserClickedRestart)
            |> Rs.s_iconsSelectionTool
                (css (highlightBackground Select)
                    :: iconsAttr "Selection tool" False UserClickedSelectionTool
                )
            |> Rs.s_iconsDisplayConfiguration
                (id (toolbarHovercardTypeToId Settings)
                    :: iconsAttr "Display settings" False (ChangedDisplaySettingsMsg UserClickedToggleDisplaySettings)
                )
            |> Rs.s_iconsCenterGraph
                (iconsAttr "Center graph" False UserClickedFitGraph)
            |> Rs.s_iconsSave
                (iconsAttr "Save file" False (UserClickedSaveGraph Nothing))
            |> Rs.s_iconsScrennshot
                (iconsAttr "Screenshot" False (UserClickedExportGraphAsImage config.exportName))
            |> Rs.s_iconsOpen
                (iconsAttr "Open" False UserClickedOpenGraph)
        )
        { iconsRedo =
            { variant =
                Icons.iconsRedoWithAttributes
                    (Icons.iconsRedoAttributes
                        |> Rs.s_root
                            (iconsAttr "Redo" config.redoDisabled UserClickedRedo)
                    )
                    { root = { state = Icons.IconsRedoStateActive } }
            }
        , iconsUndo =
            { variant =
                Icons.iconsUndoWithAttributes
                    (Icons.iconsUndoAttributes
                        |> Rs.s_root
                            (iconsAttr "Undo" config.undoDisabled UserClickedUndo)
                    )
                    { root = { state = Icons.IconsUndoStateActive } }
            }
        , root = { highlightVisible = False }
        }
