module View.Dialog exposing (body, headRow, part, view)

import Config.View exposing (Config)
import Css.Dialog as Css
import Css.View
import FontAwesome
import Html.Styled exposing (Html, button, div, h4, li, span, text, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick, stopPropagationOn)
import Json.Decode
import Model exposing (Msg(..))
import Model.Dialog exposing (ConfirmConfig, CustomConfig, ErrorConfig, ErrorType(..), InfoConfig, Model(..), OptionsConfig, PluginConfig)
import Plugin.Model
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import Theme.Html.Buttons as Buttons
import Theme.Html.ErrorMessagesAlerts
    exposing
        ( dialogConfirmationMessageAttributes
        , dialogConfirmationMessageInstances
        , dialogConfirmationMessageWithAttributes
        , dialogConfirmationMessageWithInstances
        , errorMessageComponentTypeAlertAttributes
        , errorMessageComponentTypeAlertWithAttributes
        , errorMessageComponentTypeErrorAttributes
        , errorMessageComponentTypeErrorInstances
        , errorMessageComponentTypeErrorWithInstances
        )
import Theme.Html.Icons as Icons
import Util.View exposing (addDot, none, onClickWithStop)
import View.Locale as Locale
import View.Pathfinder.TagDetailsList as TagsDetailList


view : Plugins -> Plugin.Model.ModelState -> Config -> Model Msg -> Html Msg
view plugins pluginStates vc model =
    div
        [ stopPropagationOn "click" (Json.Decode.succeed ( NoOp, True ))
        ]
        [ case model of
            Confirm conf ->
                confirm vc conf

            Options conf ->
                options_ vc conf

            Error conf ->
                error vc conf

            Info conf ->
                info vc conf

            Custom conf ->
                custom conf

            TagsList conf ->
                TagsDetailList.view vc conf.closeMsg conf.id conf.tagsTable

            Plugin conf ->
                plugin plugins pluginStates vc conf
        ]


confirm : Config -> ConfirmConfig Msg -> Html Msg
confirm vc { message, onYes, onNo, title, confirmText, cancelText } =
    let
        buttonAttrYes =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm onYes) ]

        buttonAttrNo =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm onNo) ]

        ybtn =
            Buttons.buttonTypeTextStateRegularStylePrimaryWithAttributes
                (Buttons.buttonTypeTextStateRegularStylePrimaryAttributes |> Rs.s_button buttonAttrYes)
                { typeTextStateRegularStylePrimary = { buttonText = Locale.string vc.locale (confirmText |> Maybe.withDefault "Yes"), iconInstance = none, iconVisible = True } }

        nbtn =
            Buttons.buttonTypeTextStateRegularStyleOutlinedWithAttributes
                (Buttons.buttonTypeTextStateRegularStyleOutlinedAttributes |> Rs.s_button buttonAttrNo)
                { typeTextStateRegularStyleOutlined = { buttonText = Locale.string vc.locale (cancelText |> Maybe.withDefault "No"), iconInstance = none, iconVisible = True } }
    in
    dialogConfirmationMessageWithAttributes
        (dialogConfirmationMessageAttributes |> Rs.s_iconsCloseBlack buttonAttrNo)
        { cancelButton = { variant = nbtn }, confirmButton = { variant = ybtn }, dialogConfirmationMessage = { bodyText = Locale.string vc.locale message, headerText = Locale.string vc.locale title } }


options_ : Config -> OptionsConfig Msg -> Html Msg
options_ vc { message, options } =
    let
        buttonAttrNo =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedOption NoOp) ]

        btn ( title, msg ) =
            Buttons.buttonTypeTextStateRegularStylePrimaryWithAttributes
                (Buttons.buttonTypeTextStateRegularStylePrimaryAttributes |> Rs.s_button [ css (Css.btnBase vc), onClickWithStop (UserClickedOption msg) ])
                { typeTextStateRegularStylePrimary = { buttonText = Locale.string vc.locale title, iconInstance = none, iconVisible = True } }

        btns =
            options |> List.map btn |> div [ Css.optionsButtonsContainer |> css ]
    in
    dialogConfirmationMessageWithInstances
        (dialogConfirmationMessageAttributes |> Rs.s_iconsCloseBlack buttonAttrNo)
        (dialogConfirmationMessageInstances |> Rs.s_buttonsLayout (Just btns))
        { cancelButton = { variant = none }, confirmButton = { variant = none }, dialogConfirmationMessage = { bodyText = message, headerText = Locale.string vc.locale "Please select..." } }


part : Config -> String -> List (Html msg) -> Html msg
part vc title content =
    div
        [ Css.part vc |> css
        ]
        (h4
            [ Css.heading vc |> css
            ]
            [ Locale.string vc.locale title
                |> text
            ]
            :: content
        )


headRow : Config -> String -> Maybe msg -> Html msg
headRow vc title onClose =
    div
        [ Css.headRow vc |> css
        ]
        [ title
            |> Locale.string vc.locale
            |> text
            |> List.singleton
            |> span
                [ Css.headRowText vc |> css
                ]
        , onClose
            |> Maybe.map
                (\click ->
                    button
                        [ Css.headRowClose vc |> css
                        , onClick click
                        ]
                        [ FontAwesome.icon FontAwesome.times
                            |> Html.Styled.fromUnstyled
                        ]
                )
            |> Maybe.withDefault Util.View.none
        ]


body : Config -> { onSubmit : msg } -> List (Html msg) -> Html msg
body vc { onSubmit } =
    Html.Styled.form
        [ Css.body vc |> css
        , Html.Styled.Events.onSubmit onSubmit
        ]


error : Config -> ErrorConfig Msg -> Html Msg
error vc err =
    let
        title =
            case err.type_ of
                AddressNotFound addrs ->
                    if List.length addrs > 1 then
                        "Addresses not found"

                    else
                        "Address not found"

                Http titl _ ->
                    titl

                General config ->
                    config.title

        take =
            3

        details =
            case err.type_ of
                General { message, variables } ->
                    Locale.interpolated vc.locale message variables
                        |> text
                        |> List.singleton
                        |> Util.View.p vc []
                        |> List.singleton

                Http _ e ->
                    Locale.httpErrorToString vc.locale e
                        |> text
                        |> List.singleton
                        |> Util.View.p vc []
                        |> List.singleton

                AddressNotFound addrs ->
                    [ addrs
                        |> List.take take
                        |> List.map (text >> List.singleton >> li [ Css.View.listItem vc |> css ])
                        |> (\lis ->
                                if List.length addrs > take then
                                    (List.length addrs - take)
                                        |> String.fromInt
                                        |> List.singleton
                                        |> Locale.interpolated vc.locale "... and {0} more"
                                        |> text
                                        |> List.singleton
                                        |> li []
                                        |> List.singleton
                                        |> (++) lis

                                else
                                    lis
                           )
                        |> ul []
                        |> List.singleton
                        |> Util.View.p vc []
                    , div
                        []
                        [ Locale.string vc.locale "There could be various reasons"
                            |> (\s -> s ++ ":")
                            |> text
                            |> List.singleton
                            |> Util.View.p vc []
                        , ul
                            []
                            [ li [ Css.View.listItem vc |> css ]
                                [ (if List.length addrs > 1 then
                                    "There are no transactions associated with these addresses and they are therefore not found on the blockchain"

                                   else
                                    "There are no transactions associated with this address and it is therefore not found on the blockchain"
                                  )
                                    |> Locale.string vc.locale
                                    |> addDot
                                    |> text
                                ]
                            , li
                                [ Css.View.listItem vc |> css ]
                                [ (if List.length addrs > 1 then
                                    "They are possibly not yet in our database"

                                   else
                                    "It is possibly not yet in our database"
                                  )
                                    |> Locale.string vc.locale
                                    |> addDot
                                    |> text
                                ]
                            , li [ Css.View.listItem vc |> css ]
                                [ Locale.string vc.locale "There are typos"
                                    |> addDot
                                    |> text
                                ]
                            ]
                            |> List.singleton
                            |> Util.View.p vc []
                        ]
                    ]

        icon =
            Icons.iconsError {}

        buttonAttrOk =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm err.onOk) ]
    in
    errorMessageComponentTypeErrorWithInstances
        (errorMessageComponentTypeErrorAttributes |> Rs.s_iconsCloseSnoPadding buttonAttrOk)
        (errorMessageComponentTypeErrorInstances |> Rs.s_messageText (Just (div [] details)))
        { header = { iconInstance = icon, title = Locale.string vc.locale title }, messageText = { messageText = "" }, typeError = { bodyText = "", headlineText = "" } }


info : Config -> InfoConfig Msg -> Html Msg
info vc inf =
    let
        buttonAttrOk =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm inf.onOk) ]

        icon =
            Icons.iconsAlert {}
    in
    errorMessageComponentTypeAlertWithAttributes
        (errorMessageComponentTypeAlertAttributes |> Rs.s_iconsCloseSnoPadding buttonAttrOk)
        { header = { iconInstance = icon, title = Locale.string vc.locale (inf.title |> Maybe.withDefault "Information") }, messageText = { messageText = Locale.string vc.locale inf.info }, typeAlert = { bodyText = "", headlineText = "" } }


custom : CustomConfig Msg -> Html Msg
custom { html } =
    html


plugin : Plugins -> Plugin.Model.ModelState -> Config -> PluginConfig Msg -> Html Msg
plugin plugins pluginStates vc { dialog } =
    Plugin.dialog plugins pluginStates vc dialog
