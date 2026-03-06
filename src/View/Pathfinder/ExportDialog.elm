module View.Pathfinder.ExportDialog exposing (view)

import Config.View as View
import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (autocomplete, css, placeholder, spellcheck, value)
import Html.Styled.Events exposing (onBlur, onClick, onInput)
import Model exposing (Msg(..))
import Model.Dialog exposing (ExportArea(..), ExportConfig, ExportFormat(..))
import Msg.ExportDialog exposing (Msg(..))
import RecordSetter as Rs
import Theme.Html.Dialogs as Dialogs
import Theme.Html.Fields as F
import Util.Css exposing (alignItemsStretch)
import Util.View exposing (inputFieldStyles)
import View.Button as Button
import View.Controls as Controls
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
                { radioItemsList =
                    [ Controls.radioSmall
                        (Locale.string vc.locale "Export-dialog-area-visible")
                        (model.area == ExportAreaVisible)
                        (UserClickedAreaOption ExportAreaVisible)
                    , Controls.radioSmall
                        (Locale.string vc.locale "Export-dialog-area-selected")
                        (model.area == ExportAreaSelected)
                        (UserClickedAreaOption ExportAreaSelected)
                    , Controls.radioSmall
                        (Locale.string vc.locale "Export-dialog-area-whole")
                        (model.area == ExportAreaWhole)
                        (UserClickedAreaOption ExportAreaWhole)
                    ]
                }
                { root = { title = Locale.string vc.locale "Export-dialog-area-title" |> Locale.titleCase vc.locale }
                }

        displayOptions =
            F.optionsWithTitle
                { optionsList =
                    [ Controls.checkboxLargeWithLabel
                        { label = Locale.string vc.locale "Export-dialog-display-keephighlight"
                        , checked = model.keepSelectionHighlight
                        , disabled = model.fileFormat == ExportFormatCSV
                        , msg = UserClickedKeepSelected
                        }
                    ]
                }
                { root = { title = Locale.string vc.locale "Export-dialog-display-title" |> Locale.titleCase vc.locale }
                }

        formatSelect =
            F.radioOptionsWithTitle
                { radioItemsList =
                    [ Controls.radioSmall
                        "PDF"
                        (model.fileFormat == ExportFormatPDF)
                        (UserClickedFormatOption ExportFormatPDF)
                    , Controls.radioSmall
                        "PNG"
                        (model.fileFormat == ExportFormatPNG)
                        (UserClickedFormatOption ExportFormatPNG)
                    , Controls.radioSmall
                        "CSV"
                        (model.fileFormat == ExportFormatCSV)
                        (UserClickedFormatOption ExportFormatCSV)
                    ]
                }
                { root = { title = Locale.string vc.locale "Export-dialog-format-title" |> Locale.titleCase vc.locale }
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
                            , onInput UserInputsFilename
                            , onBlur UserLeavesFilename
                            , css (inputFieldStyles False)
                            , placeholder <| Locale.string vc.locale "Export-dialog-filename-placeholder"
                            ]
                            []
                    }
                }

        disabled =
            -- TODO move to central validation function
            String.isEmpty model.filename
                || model.exporting

        buttonText =
            Locale.string vc.locale <|
                if model.exporting then
                    "Export-dialog-button-loading"

                else
                    "Export-dialog-button"
    in
    Dialogs.dialogGenericWithAttributes
        (Dialogs.dialogGenericAttributes
            |> Rs.s_iconsCloseBlack [ Util.View.pointer, onClick model.closeMsg ]
        )
        { inputList =
            [ formatSelect
            , areaSelect
            , displayOptions
            , filenameText
            ]
                |> List.map (Html.map ExportDialogMsg)
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
                    |> Rs.s_text buttonText
                    |> Rs.s_disabled disabled
                    |> Rs.s_onClick (UserClickedExport |> ExportDialogMsg |> Just)
                )
                    |> Button.primaryButton vc
            }
        , root =
            { header = Locale.string vc.locale "Export-dialog-title" |> Locale.titleCase vc.locale
            , description = Locale.string vc.locale "Export-dialog-description"
            }
        }
