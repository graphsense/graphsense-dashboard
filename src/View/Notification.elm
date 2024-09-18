module View.Notification exposing (..)

import Config.View as View
import Css
import Css.Dialog as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model exposing (Msg(..))
import Model.Notification exposing (..)
import RecordSetter exposing (..)
import Theme.Html.Buttons exposing (..)
import Theme.Html.ErrorMessagesAlerts exposing (errorMessageComponentProperty1AlertAttributes, errorMessageComponentProperty1AlertWithAttributes, errorMessageComponentProperty1ErrorAttributes, errorMessageComponentProperty1ErrorInstances, errorMessageComponentProperty1ErrorWithInstances)
import Theme.Html.Icons as Icons
import Tuple exposing (..)
import Util.View exposing (none, onClickWithStop)
import View.Locale as Locale


view : View.Config -> Model -> Html Msg
view vc model =
    let
        not =
            model |> getNext

        overlay =
            div [ css [ Css.position Css.relative, Css.left (Css.px 80), Css.bottom (Css.px 100) ] ]
    in
    case not of
        Just (Error title text) ->
            let
                icon =
                    Icons.iconsError {}

                buttonAttrOk =
                    [ css (Css.btnBase vc), onClickWithStop (UserClickedConfirm UserClosesNotification) ]
            in
            errorMessageComponentProperty1ErrorWithInstances
                (errorMessageComponentProperty1ErrorAttributes |> s_iconsCloseSmall buttonAttrOk)
                errorMessageComponentProperty1ErrorInstances
                { header = { iconInstance = icon, title = Locale.string vc.locale title }, messageText = { messageText = Locale.string vc.locale text }, property1Error = { bodyText = "", headlineText = "" } }
                |> List.singleton
                |> overlay

        Just (Info title text) ->
            let
                buttonAttrOk =
                    [ css (Css.btnBase vc), onClickWithStop UserClosesNotification ]

                icon =
                    Icons.iconsAlert {}
            in
            errorMessageComponentProperty1AlertWithAttributes
                (errorMessageComponentProperty1AlertAttributes |> s_iconsCloseSmall buttonAttrOk)
                { header = { iconInstance = icon, title = Locale.string vc.locale title }, messageText = { messageText = Locale.string vc.locale text }, property1Alert = { bodyText = "", headlineText = "" } }
                |> List.singleton
                |> overlay

        Nothing ->
            none
