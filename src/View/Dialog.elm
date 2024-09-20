module View.Dialog exposing (..)

import Config.View exposing (Config)
import Css.Dialog as Css
import Css.View
import FontAwesome
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode
import Model exposing (Msg(..))
import Model.Dialog exposing (..)
import RecordSetter exposing (..)
import Theme.Html.Buttons exposing (..)
import Theme.Html.ErrorMessagesAlerts
    exposing
        ( dialogConfirmationMessageAttributes
        , dialogConfirmationMessageInstances
        , dialogConfirmationMessageWithAttributes
        , dialogConfirmationMessageWithInstances
        , errorMessageComponentProperty1AlertAttributes
        , errorMessageComponentProperty1AlertWithAttributes
        , errorMessageComponentProperty1ErrorAttributes
        , errorMessageComponentProperty1ErrorInstances
        , errorMessageComponentProperty1ErrorWithInstances
        )
import Theme.Html.Icons as Icons
import Util.View exposing (addDot, none, onClickWithStop)
import View.Locale as Locale


view : Config -> Model Msg -> Html Msg
view vc model =
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
        ]


confirm : Config -> ConfirmConfig Msg -> Html Msg
confirm vc { message, onYes, onNo, title, confirmText, cancelText } =
    let
        buttonAttrYes =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm onYes) ]

        buttonAttrNo =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm onNo) ]

        ybtn =
            buttonStyleColoredStateRegularWithAttributes
                (buttonStyleColoredStateRegularAttributes |> s_button buttonAttrYes)
                { styleColoredStateRegular = { buttonText = Locale.string vc.locale (confirmText |> Maybe.withDefault "Yes"), iconInstance = none, iconVisible = True } }

        nbtn =
            buttonStyleOutlineStateRegularWithAttributes
                (buttonStyleOutlineStateRegularAttributes |> s_button buttonAttrNo)
                { styleOutlineStateRegular = { buttonText = Locale.string vc.locale (cancelText |> Maybe.withDefault "No"), iconInstance = none, iconVisible = True } }
    in
    dialogConfirmationMessageWithAttributes
        (dialogConfirmationMessageAttributes |> s_iconsCloseBlack buttonAttrNo)
        { cancelButton = { variant = nbtn }, confirmButton = { variant = ybtn }, dialogConfirmationMessage = { bodyText = message, headerText = title } }


options_ : Config -> OptionsConfig Msg -> Html Msg
options_ vc { message, options } =
    let
        buttonAttrNo =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedOption NoOp) ]

        btn ( title, msg ) =
            buttonStyleColoredStateRegularWithAttributes
                (buttonStyleColoredStateRegularAttributes |> s_button [ css (Css.btnBase vc), onClickWithStop (UserClickedOption msg) ])
                { styleColoredStateRegular = { buttonText = Locale.string vc.locale title, iconInstance = none, iconVisible = True } }

        btns =
            options |> List.map btn |> div [ Css.optionsButtonsContainer |> css ]
    in
    dialogConfirmationMessageWithInstances
        (dialogConfirmationMessageAttributes |> s_iconsCloseBlack buttonAttrNo)
        (dialogConfirmationMessageInstances |> s_buttonsLayout (Just btns))
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
    errorMessageComponentProperty1ErrorWithInstances
        (errorMessageComponentProperty1ErrorAttributes |> s_iconsCloseSmall buttonAttrOk)
        (errorMessageComponentProperty1ErrorInstances |> s_messageText (Just (div [] details)))
        { header = { iconInstance = icon, title = Locale.string vc.locale title }, messageText = { messageText = "" }, property1Error = { bodyText = "", headlineText = "" } }


info : Config -> InfoConfig Msg -> Html Msg
info vc inf =
    let
        buttonAttrOk =
            [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm inf.onOk) ]

        icon =
            Icons.iconsAlert {}
    in
    errorMessageComponentProperty1AlertWithAttributes
        (errorMessageComponentProperty1AlertAttributes |> s_iconsCloseSmall buttonAttrOk)
        { header = { iconInstance = icon, title = Locale.string vc.locale (inf.title |> Maybe.withDefault "Information") }, messageText = { messageText = inf.info }, property1Alert = { bodyText = "", headlineText = "" } }
