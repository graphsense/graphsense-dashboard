module View.Notification exposing (view)

import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.Dialog as Css
import Css.Transitions
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Maybe.Extra
import Model exposing (Msg(..))
import Model.Notification as Notification
import RecordSetter as Rs
import Theme.Html.ErrorMessagesAlerts as Msg
    exposing
        ( errorMessageComponentTypeAlertAttributes
        , errorMessageComponentTypeAlertWithAttributes
        , errorMessageComponentTypeErrorAttributes
        , errorMessageComponentTypeErrorWithAttributes
        , errorMessageComponentTypeSuccessAttributes
        , errorMessageComponentTypeSuccessWithAttributes
        )
import Theme.Html.Icons as Icons
import Theme.Html.Navbar as Nb
import Util.Css
import Util.View exposing (fixFillRule, none, onClickWithStop)
import View.Locale as Locale


overlay : Bool -> List (Html msg) -> Html msg
overlay moved =
    div
        [ css
            [ Css.position Css.absolute
            , Css.left (Css.px (Nb.navbarMenuNew_details.renderedWidth + 5))
            , Css.Transitions.bottom 200
                |> List.singleton
                |> Css.Transitions.transition
            , Css.bottom <|
                Css.px <|
                    if moved then
                        30

                    else
                        -100
            , Util.Css.zIndexMain
            ]
        ]


view : View.Config -> Notification.Model -> Html Msg
view vc model =
    let
        not =
            model |> Notification.peek

        hide =
            [ css [ Css.display Css.none ] ]

        closeBtnAttr =
            [ css (Css.btnBase vc), onClickWithStop UserClosesNotification ]

        contentAttr =
            [ css [ Css.width Css.auto, Css.maxWidth (Css.px Msg.messageText_details.renderedWidth) ] ]

        notificationViewConfig { title, message, moreInfo, variables, showClose } =
            let
                -- showHeader =
                --     showClose || Maybe.Extra.isJust title
                showMsgText =
                    Maybe.Extra.isJust title

                msgText =
                    message
                        :: moreInfo
                        |> List.map (flip (Locale.interpolated vc.locale) variables)
                        |> String.join " "
            in
            { msg =
                msgText
            , btnOkAttr =
                if showClose then
                    closeBtnAttr

                else
                    hide
            , headerFrameAttr =
                if showClose then
                    []

                else
                    hide
            , title = title |> Maybe.withDefault msgText |> flip (Locale.interpolated vc.locale) variables
            , msgTextAttr =
                if showMsgText then
                    []

                else
                    hide
            }
    in
    case not of
        Just (Notification.Error nd) ->
            let
                icon =
                    Icons.iconsError {}

                nvc =
                    notificationViewConfig nd
            in
            errorMessageComponentTypeErrorWithAttributes
                (errorMessageComponentTypeErrorAttributes
                    |> Rs.s_headerFrame nvc.headerFrameAttr
                    |> Rs.s_messageText nvc.msgTextAttr
                    |> Rs.s_iconsCloseSnoPadding nvc.btnOkAttr
                    |> Rs.s_content contentAttr
                )
                { header =
                    { iconInstance = icon
                    , title = nvc.title
                    }
                , root = { bodyText = "", headlineText = "" }
                , messageText = { messageText = nvc.msg }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        Just (Notification.Info nd) ->
            let
                icon =
                    Icons.iconsAlert {}

                nvc =
                    notificationViewConfig nd
            in
            errorMessageComponentTypeAlertWithAttributes
                (errorMessageComponentTypeAlertAttributes
                    |> Rs.s_headerFrame nvc.headerFrameAttr
                    |> Rs.s_messageText nvc.msgTextAttr
                    |> Rs.s_iconsCloseSnoPadding nvc.btnOkAttr
                    |> Rs.s_content contentAttr
                )
                { header =
                    { iconInstance = icon
                    , title = nvc.title
                    }
                , root = { bodyText = "", headlineText = "" }
                , messageText = { messageText = nvc.msg }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        Just (Notification.Success nd) ->
            let
                icon =
                    Icons.iconsAlertDoneWithAttributes
                        (Icons.iconsAlertDoneAttributes
                            |> Rs.s_subtract [ fixFillRule ]
                        )
                        {}

                nvc =
                    notificationViewConfig nd
            in
            errorMessageComponentTypeSuccessWithAttributes
                (errorMessageComponentTypeSuccessAttributes
                    |> Rs.s_headerFrame nvc.headerFrameAttr
                    |> Rs.s_messageText nvc.msgTextAttr
                    |> Rs.s_iconsCloseSnoPadding nvc.btnOkAttr
                    |> Rs.s_content contentAttr
                )
                { header =
                    { iconInstance = icon
                    , title = nvc.title
                    }
                , root = { bodyText = "", headlineText = "" }
                , messageText = { messageText = nvc.msg }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        Nothing ->
            none
