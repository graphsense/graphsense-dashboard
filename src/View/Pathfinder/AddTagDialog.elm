module View.Pathfinder.AddTagDialog exposing (view)

import Config.View as View
import Css
import Html.Styled as Html exposing (Html, textarea)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (AddTagDialogMsgs(..), Msg(..))
import Model.Dialog as Dialog
import Plugin.View exposing (Plugins)
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.Dialogs as Dialogs
import Theme.Html.Fields as F
import Util.View
import View.Button as Button
import View.Locale as Locale
import View.Search


view : Plugins -> View.Config -> Dialog.AddTagConfig Msg -> Html Msg
view plugins vc model =
    let
        searchInput =
            View.Search.searchWithMoreCss plugins
                vc
                (View.Search.default
                    |> Rs.s_css
                        (\_ ->
                            Css.outline Css.none
                                -- :: Css.pseudoClass "placeholder" Sc.searchBarFieldStatePlaceholderSearchInputField_details.styles
                                :: (Css.width <| Css.pct 100)
                                :: Util.View.inputFieldStyles False
                        )
                    |> Rs.s_formCss
                        [ Css.flexGrow <| Css.num 1
                        , Css.height Css.auto |> Css.important
                        ]
                    |> Rs.s_frameCss
                        [ Css.height <| Css.pct 100
                        , Css.marginRight Css.zero |> Css.important
                        ]
                    |> Rs.s_resultLine
                        [ Css.property "background-color" Colors.white
                        , Css.hover
                            [ Css.property "background-color" Colors.greyBlue50
                                |> Css.important
                            ]
                        ]
                    |> Rs.s_resultLineHighlighted
                        [ Css.property "background-color" Colors.greyBlue50
                        ]
                    |> Rs.s_resultsAsLink True
                    |> Rs.s_dropdownResult
                        [ Css.property "background-color" Colors.white
                        ]
                    |> Rs.s_dropdownFrame
                        [ Css.property "background-color" Colors.white
                        ]
                )
                model.search
                |> Html.map (SearchMsgAddTagDialog >> AddTagDialog)

        actorText =
            F.textFieldWithHelp
                { root =
                    { helpText = Locale.string vc.locale "Start typing to search existing labels."
                    , state = F.TextFieldWithHelpStateDefault
                    , title = Locale.string vc.locale "Actor Label *"
                    }
                , textField = { variant = searchInput }
                }

        additionalInfo =
            F.textFieldWithHelp
                { root =
                    { helpText = Locale.string vc.locale "Add context, notes, or links to supporting evidence."
                    , state = F.TextFieldWithHelpStateDefault
                    , title = Locale.string vc.locale "Additional Information (optional)"
                    }
                , textField = { variant = textarea [ Util.View.inputFieldStyles False |> css, value model.description, onInput (UserInputsDescription >> AddTagDialog) ] [] }
                }
    in
    Dialogs.dialogAddTagWithAttributes
        (Dialogs.dialogAddTagAttributes
            |> Rs.s_iconsCloseBlack [ Util.View.pointer, onClick model.closeMsg ]
        )
        { actorLabel = { variant = actorText }
        , additionalInfo = { variant = additionalInfo }
        , cancelButton =
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
                    |> Rs.s_text "Add Tag"
                    |> Rs.s_onClick (Just model.closeMsg)
                )
                    |> Button.primaryButton vc
            }
        , root = { header = Locale.string vc.locale "Add Tag to Address" }
        }
