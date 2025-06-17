module View.Pathfinder.AddTagDialog exposing (view)

import Config.View as View
import Css
import Html.Styled as Html exposing (Html, textarea)
import Html.Styled.Attributes exposing (css, placeholder, value)
import Html.Styled.Events exposing (onClick, onInput)
import Model exposing (AddTagDialogMsgs(..), Msg(..))
import Model.Dialog as Dialog
import Plugin.View exposing (Plugins)
import RecordSetter as Rs
import Theme.Colors as Colors
import Theme.Html.Dialogs as Dialogs
import Theme.Html.Fields as F
import Theme.Html.SettingsComponents as Sc
import Util.Css exposing (alignItemsStretch)
import Util.View
import View.Button as Button
import View.Locale as Locale
import View.Search


view : Plugins -> View.Config -> Dialog.AddTagConfig Msg -> Html Msg
view plugins vc model =
    let
        actorField =
            case model.selectedActor of
                Just ( _, name ) ->
                    Dialogs.actorTagWithAttributes
                        (Dialogs.actorTagAttributes
                            |> Rs.s_iconsCloseSnoPadding
                                [ Util.View.pointer
                                , onClick (RemoveActorTag |> AddTagDialog)
                                ]
                        )
                        { root = { closeVisible = True, label = name } }

                _ ->
                    View.Search.searchWithMoreCss plugins
                        vc
                        (View.Search.default
                            |> Rs.s_showIcon False
                            |> Rs.s_resultGroupTitle [ Css.display Css.none ]
                            |> Rs.s_resultLineIcon [ Css.display Css.none ]
                            |> Rs.s_css
                                (\_ ->
                                    Css.outline Css.none
                                        :: Css.pseudoClass "placeholder" Sc.searchBarFieldStatePlaceholderSearchInputField_details.styles
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
                                , Css.height (Css.px 20)
                                , Css.displayFlex |> Css.important
                                , Css.width Css.auto
                                , Css.alignItems Css.center
                                , Css.paddingLeft (Css.px 5)
                                , Css.hover
                                    [ Css.property "background-color" Colors.greyBlue50
                                        |> Css.important
                                    , Css.borderRadius (Css.px 5)
                                    ]
                                ]
                            |> Rs.s_resultLineHighlighted
                                [ Css.property "background-color" Colors.greyBlue50
                                , Css.borderRadius (Css.px 5)
                                ]
                            |> Rs.s_resultsAsLink True
                            |> Rs.s_dropdownResult
                                [ Css.property "background-color" Colors.white
                                ]
                            |> Rs.s_dropdownFrame
                                [ Css.property "background-color" Colors.white
                                ]
                            |> Rs.s_inputAttributes [ placeholder (Locale.string vc.locale "e.g. Binance") ]
                        )
                        model.search
                        |> Html.map (SearchMsgAddTagDialog >> AddTagDialog)

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

        actorText =
            F.textFieldWithHelpWithAttributes
                textFieldAttributes
                { root =
                    { helpText = Locale.string vc.locale "Start typing to search existing labels."
                    , state = F.TextFieldWithHelpStateDefault
                    , title = Locale.string vc.locale "Actor Label *"
                    }
                , textField = { variant = actorField }
                }

        additionalInfo =
            F.textFieldWithHelpWithAttributes
                textFieldAttributes
                { root =
                    { helpText = Locale.string vc.locale "Add context, notes, or links to supporting evidence."
                    , state = F.TextFieldWithHelpStateDefault
                    , title = Locale.string vc.locale "Additional Information (optional)"
                    }
                , textField =
                    { variant =
                        textarea
                            [ Util.View.inputFieldStyles False |> css
                            , [ Css.resize Css.none
                              , Css.height (Css.em 5) |> Css.important
                              , Css.focus [ Css.height (Css.em 5) |> Css.important ]
                              , Css.whiteSpace Css.preLine
                              ]
                                |> css
                            , value model.description
                            , placeholder (Locale.string vc.locale "e.g. Linked to scam report on xyz.com")
                            , onInput (UserInputsDescription >> AddTagDialog)
                            ]
                            []
                    }
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
                    |> Rs.s_disabled (model.selectedActor == Nothing)
                    |> Rs.s_onClick (Just model.addTagMsg)
                )
                    |> Button.primaryButton vc
            }
        , root = { header = Locale.string vc.locale "Add Tag to Address" }
        }
