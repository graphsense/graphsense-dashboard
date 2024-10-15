module View.Notification exposing (view)

import Config.View as View
import Css
import Css.Dialog as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (Msg(..))
import Model.Notification exposing (..)
import RecordSetter exposing (..)
import Theme.Html.ErrorMessagesAlerts
    exposing
        ( errorMessageComponentProperty1AlertAttributes
        , errorMessageComponentProperty1AlertWithAttributes
        , errorMessageComponentProperty1ErrorAttributes
        , errorMessageComponentProperty1ErrorWithAttributes
        )
import Theme.Html.Icons as Icons
import Theme.Html.Navbar as Nb
import Util.Css as Css
import Util.View exposing (none, onClickWithStop)
import View.Locale as Locale


overlay : List (Html msg) -> Html msg
overlay =
    div
        [ css
            [ Css.position Css.absolute
            , Css.left (Css.px (Nb.navbarMenuNew_details.renderedWidth + 5))
            , Css.bottom (Css.px 30)
            , Css.zIndex (Css.int (Css.zIndexMainValue + 10))
            ]
        ]


view : View.Config -> Model -> Html Msg
view vc model =
    let
        not =
            model |> peek
    in
    case not of
        Just (Error { title, message, variables }) ->
            let
                icon =
                    Icons.iconsError {}

                buttonAttrOk =
                    [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm UserClosesNotification) ]
            in
            errorMessageComponentProperty1ErrorWithAttributes
                (errorMessageComponentProperty1ErrorAttributes
                    |> s_iconsCloseSmall buttonAttrOk
                )
                { header =
                    { iconInstance = icon
                    , title = Locale.string vc.locale title
                    }
                , messageText =
                    { messageText = Locale.interpolated vc.locale message variables }
                , property1Error =
                    { bodyText = ""
                    , headlineText = ""
                    }
                }
                |> List.singleton
                |> overlay

        Just (Info { title, message, variables }) ->
            let
                buttonAttrOk =
                    [ css (Css.btnBase vc), onClickWithStop UserClosesNotification ]

                icon =
                    Icons.iconsAlert {}
            in
            errorMessageComponentProperty1AlertWithAttributes
                (errorMessageComponentProperty1AlertAttributes |> s_iconsCloseSmall buttonAttrOk)
                { header =
                    { iconInstance = icon
                    , title = Locale.string vc.locale title
                    }
                , messageText = { messageText = Locale.interpolated vc.locale message variables }
                , property1Alert = { bodyText = "", headlineText = "" }
                }
                |> List.singleton
                |> overlay

        Nothing ->
            none
