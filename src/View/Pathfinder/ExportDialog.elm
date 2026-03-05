module View.Pathfinder.ExportDialog exposing (view)

import Config.View as View
import Css
import Html.Styled as Html exposing (Html, textarea)
import Html.Styled.Attributes exposing (autocomplete, css, placeholder, spellcheck, value)
import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (Msg(..))
import Model.Dialog exposing (ExportConfig)
import Msg.ExportDialog exposing (Msg(..))
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.Dialogs as Dialogs
import Theme.Html.Fields as F
import Theme.Html.SettingsComponents as Sc
import Tuple exposing (second)
import Util.Css exposing (alignItemsStretch)
import Util.View exposing (inputFieldStyles)
import View.Button as Button
import View.Locale as Locale


view : View.Config -> ExportConfig Model.Msg -> Html Model.Msg
view vc model =
    let
        textFieldAttributes =
            F.textFieldWithHelpAttributes
                |> Rs.s_root
                    [ [ Css.width <| Css.pct 90 ] |> css
                    , alignItemsStretch
                    ]
                |> Rs.s_helperText
                    [ [ Css.property "white-space" "wrap" |> Css.important
                      ]
                        |> css
                    ]

        areaSelect =
            F.radioOptionsWithTitle
                { radioItemsList = [] }
                { root = { title = Locale.string vc.locale "Export-dialog-area-title" }
                }

        keepHighlightSwitch =
            F.optionsWithTitle
                { optionsList = [] }
                { root = { title = Locale.string vc.locale "Export-dialog-keephighlight-title" }
                }

        formatSelect =
            F.radioOptionsWithTitle
                { radioItemsList = [] }
                { root = { title = Locale.string vc.locale "Export-dialog-format-title" }
                }

        filenameText =
            F.textFieldWithHelpWithAttributes
                textFieldAttributes
                { root =
                    { helpText = Locale.string vc.locale "Export-dialog-filename-help"
                    , state = F.TextFieldWithHelpStateDefault
                    , title = (Locale.string vc.locale "Export-dialog-filename-label" |> Locale.titleCase vc.locale) ++ " *"
                    }
                , textField =
                    { variant =
                        Html.input
                            [ value model.filename
                            , spellcheck False
                            , autocomplete False

                            --, onInput UserInputsCaseDescription
                            , css (inputFieldStyles False)
                            , placeholder <| Locale.string vc.locale "Export-dialog-filename-placeholder"
                            ]
                            []
                    }
                }

        invalid =
            -- TODO move to central validation function
            String.isEmpty model.filename
    in
    Dialogs.dialogGenericWithAttributes
        (Dialogs.dialogGenericAttributes
            |> Rs.s_iconsCloseBlack [ Util.View.pointer, onClick model.closeMsg ]
        )
        { inputList =
            [ areaSelect
            , keepHighlightSwitch
            , formatSelect
            , filenameText
            ]
        }
        { cancelButton =
            { variant =
                (Button.defaultConfig
                    |> Rs.s_text "Cancel"
                    |> Rs.s_onClick (Just model.closeMsg)
                )
                    |> Button.linkButtonBlue vc
            }
        , confirmButton =
            { variant =
                (Button.defaultConfig
                    |> Rs.s_text (Locale.string vc.locale "Export")
                    |> Rs.s_disabled invalid
                    |> Rs.s_onClick (UserClickedExport |> ExportDialogMsg |> Just)
                )
                    |> Button.primaryButton vc
            }
        , root =
            { header = Locale.string vc.locale "Export-dialog-title" |> Locale.titleCase vc.locale
            , description = Locale.string vc.locale "Export-dialog-description"
            }
        }
