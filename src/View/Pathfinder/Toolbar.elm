module View.Pathfinder.Toolbar exposing (..)

import Config.View as View
import Css
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as HA exposing (css, id, src)
import Html.Styled.Lazy exposing (..)
import Model.Pathfinder.Tools exposing (PointerTool(..))
import Msg.Pathfinder exposing (DisplaySettingsMsg(..), Msg(..), TxDetailsMsg(..))
import RecordSetter exposing (..)
import Theme.Colors
import Theme.Html.Icons as Icons
import Theme.Html.SettingsComponents as SettingsComponents
import Util.View exposing (none, onClickWithStop, toCssColor)
import View.Locale as Locale


type alias Config =
    { undoDisabled : Bool
    , redoDisabled : Bool
    , pointerTool : PointerTool
    }


view : View.Config -> Config -> Html Msg
view vc config =
    let
        iconsAttr =
            [ css
                [ Css.cursor Css.pointer
                ]
            ]

        highlightBackground pointer =
            [ Css.important <|
                Css.backgroundColor <|
                    if config.pointerTool == pointer then
                        Theme.Colors.brandLight
                            |> toCssColor

                    else
                        Css.rgba 0 0 0 0
            ]

        title str =
            Locale.string vc.locale str
                |> HA.title
    in
    SettingsComponents.toolbarWithInstances
        (SettingsComponents.toolbarAttributes
            |> s_iconsDelete
                (onClickWithStop UserClickedToolbarDeleteIcon
                    :: title "Delete"
                    :: iconsAttr
                )
            |> s_iconsNewFile
                (onClickWithStop UserClickedRestart
                    :: title "Restart"
                    :: iconsAttr
                )
            |> s_iconsMouseCursor
                (onClickWithStop (ChangePointerTool Select |> ChangedDisplaySettingsMsg)
                    :: title "Selection tool"
                    :: css (highlightBackground Select)
                    :: iconsAttr
                )
            |> s_iconsHand
                (onClickWithStop (ChangePointerTool Drag |> ChangedDisplaySettingsMsg)
                    :: title "Move tool"
                    :: css (highlightBackground Drag)
                    :: iconsAttr
                )
            |> s_iconsDisplayConfiguration
                (id "toolbar-display-settings"
                    :: title "Display settings"
                    :: onClickWithStop (UserClickedToggleDisplaySettings |> ChangedDisplaySettingsMsg)
                    :: iconsAttr
                )
            |> s_iconsCenterGraph
                (onClickWithStop UserClickedFitGraph
                    :: title "Center graph"
                    :: iconsAttr
                )
        )
        (SettingsComponents.toolbarInstances
            |> s_iconsRedo
                (Just <|
                    if config.redoDisabled then
                        Icons.iconsRedoStateDisabledWithAttributes
                            Icons.iconsRedoStateDisabledAttributes
                            {}

                    else
                        Icons.iconsRedoStateActiveWithAttributes
                            (Icons.iconsRedoStateActiveAttributes
                                |> s_stateActive
                                    (onClickWithStop UserClickedRedo
                                        :: title "Redo"
                                        :: iconsAttr
                                    )
                            )
                            {}
                )
            |> s_iconsUndo
                (Just <|
                    if config.undoDisabled then
                        Icons.iconsUndoWithAttributes
                            (Icons.iconsUndoAttributes
                                |> s_iconsUndo
                                    (title "Undo"
                                        :: [ css [ Css.opacity <| Css.num 0.3 ] ]
                                    )
                            )
                            {}

                    else
                        Icons.iconsUndoWithAttributes
                            (Icons.iconsUndoAttributes
                                |> s_iconsUndo
                                    (onClickWithStop UserClickedUndo
                                        :: title "Undo"
                                        :: iconsAttr
                                    )
                            )
                            {}
                )
        )
        { iconsRedo = {}
        , toolbar = { highlightVisible = False }
        }
