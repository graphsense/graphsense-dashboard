module View.Notification exposing (view)

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
import Theme.Html.ErrorMessagesAlerts
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

        notificationViewConfig { title, message, moreInfo, variables, showClose } =
            let
                showHeader =
                    showClose || Maybe.Extra.isJust title

                showMsgText =
                    Maybe.Extra.isJust title

                msgText =
                    message
                        :: moreInfo
                        |> List.map (\m -> Locale.interpolated vc.locale m variables)
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
                if showHeader then
                    []

                else
                    hide
            , title = title |> Maybe.withDefault msgText |> Locale.string vc.locale
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
                    |> Rs.s_content [ css [ Css.width Css.auto ] ]
                )
                { header =
                    { iconInstance = icon
                    , title = nvc.title
                    }
                , typeError = { bodyText = "", headlineText = "" }
                , messageText = { messageText = nvc.msg }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        -- let
        --     icon =
        --         Icons.iconsError {}
        --     buttonAttrOk =
        --         [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm UserClosesNotification) ]
        -- in
        -- errorMessageComponentTypeErrorWithAttributes
        --     (errorMessageComponentTypeErrorAttributes
        --         |> Rs.s_iconsCloseSnoPadding buttonAttrOk
        --     )
        --     { header =
        --         { iconInstance = icon
        --         , title = Locale.string vc.locale (title |> Maybe.withDefault "")
        --         }
        --     , messageText =
        --         { messageText =
        --             message
        --                 :: moreInfo
        --                 |> List.map (\m -> Locale.interpolated vc.locale m variables)
        --                 |> String.join " "
        --         }
        --     , typeError =
        --         { bodyText = ""
        --         , headlineText = ""
        --         }
        --     }
        --     |> List.singleton
        --     |> overlay (Notification.getMoved model)
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
                    |> Rs.s_content [ css [ Css.width Css.auto ] ]
                )
                { header =
                    { iconInstance = icon
                    , title = nvc.title
                    }
                , typeAlert = { bodyText = "", headlineText = "" }
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
                    |> Rs.s_content [ css [ Css.width Css.auto ] ]
                )
                { header =
                    { iconInstance = icon
                    , title = nvc.title
                    }
                , typeSuccess = { bodyText = "", headlineText = "" }
                , messageText = { messageText = nvc.msg }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        Nothing ->
            none
