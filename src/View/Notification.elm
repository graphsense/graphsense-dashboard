module View.Notification exposing (view)

import Config.View as View
import Css
import Css.Dialog as Css
import Css.Transitions
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
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
    in
    case not of
        Just (Notification.Error { title, message, moreInfo, variables }) ->
            let
                icon =
                    Icons.iconsError {}

                buttonAttrOk =
                    [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm UserClosesNotification) ]
            in
            errorMessageComponentTypeErrorWithAttributes
                (errorMessageComponentTypeErrorAttributes
                    |> Rs.s_iconsCloseSnoPadding buttonAttrOk
                )
                { header =
                    { iconInstance = icon
                    , title = Locale.string vc.locale title
                    }
                , messageText =
                    { messageText =
                        message
                            :: moreInfo
                            |> List.map (\m -> Locale.interpolated vc.locale m variables)
                            |> String.join " "
                    }
                , typeError =
                    { bodyText = ""
                    , headlineText = ""
                    }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        Just (Notification.Info { title, message, moreInfo, variables }) ->
            let
                buttonAttrOk =
                    [ css (Css.btnBase vc), onClickWithStop UserClosesNotification ]

                icon =
                    Icons.iconsAlert {}
            in
            errorMessageComponentTypeAlertWithAttributes
                (errorMessageComponentTypeAlertAttributes |> Rs.s_iconsCloseSnoPadding buttonAttrOk)
                { header =
                    { iconInstance = icon
                    , title = Locale.string vc.locale title
                    }
                , messageText =
                    { messageText =
                        message
                            :: moreInfo
                            |> List.map (\m -> Locale.interpolated vc.locale m variables)
                            |> String.join " "
                    }
                , typeAlert = { bodyText = "", headlineText = "" }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        Just (Notification.InfoEphemeral title) ->
            let
                hide =
                    [ css [ Css.display Css.none ] ]

                icon =
                    Icons.iconsAlert {}
            in
            errorMessageComponentTypeAlertWithAttributes
                (errorMessageComponentTypeAlertAttributes
                    |> Rs.s_headerFrame hide
                    |> Rs.s_messageText hide
                    |> Rs.s_content [ css [ Css.width Css.auto ] ]
                )
                { header =
                    { iconInstance = icon
                    , title = Locale.string vc.locale title
                    }
                , typeAlert = { bodyText = "", headlineText = "" }
                , messageText = { messageText = "" }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        Just (Notification.Success title) ->
            let
                hideClose =
                    [ css [ Css.display Css.none ] ]

                icon =
                    Icons.iconsAlertDoneWithAttributes
                        (Icons.iconsAlertDoneAttributes
                            |> Rs.s_subtract [ fixFillRule ]
                        )
                        {}
            in
            errorMessageComponentTypeSuccessWithAttributes
                (errorMessageComponentTypeSuccessAttributes
                    |> Rs.s_headerFrame hideClose
                )
                { header =
                    { iconInstance = icon
                    , title = Locale.string vc.locale title
                    }
                , typeSuccess = { bodyText = "", headlineText = "" }
                }
                |> List.singleton
                |> overlay (Notification.getMoved model)

        Nothing ->
            none
