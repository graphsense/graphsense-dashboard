module View.Statusbar exposing (view)

import Api
import Config.View as View
import Css
import Css.Transitions
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Http
import List.Extra
import Model exposing (Msg(..))
import Model.Statusbar exposing (..)
import RecordSetter as Rs
import Theme.Html.Icons as Icons
import Theme.Html.Page
import Tuple exposing (first, second)
import Util.View exposing (firstToUpper, fullWidthCss, pointer)
import Util.View.Loadingspinner as Loadingspinner
import Version exposing (version)
import View.Locale as Locale


view : View.Config -> Model -> Html Msg
view vc model =
    let
        entries =
            model.messages
                |> Dict.toList
                |> List.map
                    (\( id, ( key, values ) ) ->
                        ( key, values, Dict.get id model.retries )
                    )

        firstMessageText =
            entries
                |> List.head
                |> Maybe.map
                    (\( key, values, retryAttempt ) ->
                        let
                            retrySuffix =
                                case retryAttempt of
                                    Just attempt ->
                                        " ("
                                            ++ ([ String.fromInt attempt, String.fromInt 3 ]
                                                    |> Locale.interpolated vc.locale "retrying {0}/{1}"
                                               )
                                            ++ ")"

                                    Nothing ->
                                        ""
                        in
                        messageString vc key values ++ retrySuffix
                    )
                |> Maybe.withDefault ""

        messageList =
            (entries
                |> List.map
                    (\( key, values, retryAttempt ) ->
                        let
                            text_ =
                                messageString vc key values
                                    ++ (case retryAttempt of
                                            Just attempt ->
                                                " ("
                                                    ++ ([ String.fromInt attempt, String.fromInt 3 ]
                                                            |> Locale.interpolated vc.locale "retrying {0}/{1}"
                                                       )
                                                    ++ ")"

                                            Nothing ->
                                                ""
                                       )
                        in
                        Theme.Html.Page.messageElement
                            { root =
                                { text = text_
                                , iconVisible = True
                                , icon = Icons.iconsDoneS {}
                                }
                            }
                    )
            )
                ++ (model.log
                        |> List.map
                            (\logEntry ->
                                let
                                    text_ =
                                        logToText vc model.lastBlocks logEntry
                                in
                                Theme.Html.Page.messageElement
                                    { root =
                                        { text = text_
                                        , iconVisible = True
                                        , icon = Icons.iconsDoneS {}
                                        }
                                    }
                            )
                   )
    in
    if not model.visible then
        Theme.Html.Page.footerWithAttributes
            (Theme.Html.Page.footerAttributes
                |> Rs.s_root
                    [ css
                        [ Css.Transitions.transition
                            [ Css.Transitions.minHeight 200
                            , Css.Transitions.maxHeight 200
                            ]
                        , Css.minHeight <| Css.px Theme.Html.Page.footer_details.height
                        , Css.maxHeight <| Css.px Theme.Html.Page.footer_details.height
                        , fullWidthCss
                        ]
                    , onClick UserClickedStatusbar
                    ]
            )
            { messageElement =
                { text = firstMessageText
                , iconVisible = List.isEmpty entries |> not
                , icon =
                    Loadingspinner.html
                        [ css
                            [ Css.padding (Css.px 0)
                            , Css.rem 0.3 |> Css.paddingRight
                            ]
                        ]
                }
            , root = { version = version }
            }

    else
        Theme.Html.Page.footerOpenWithAttributes
            { iconsCloseNoPadding =
                [ onClick UserClickedStatusbar
                , pointer
                ]
            , messageList = []
            , root =
                [ css
                    [ Css.Transitions.transition
                        [ Css.Transitions.minHeight 200
                        , Css.Transitions.maxHeight 200
                        ]
                    , Css.minHeight <| Css.px (Theme.Html.Page.footer_details.height * 10)
                    , Css.maxHeight <| Css.px (Theme.Html.Page.footer_details.height * 10)
                    , fullWidthCss
                    ]
                ]
            , vector = []
            }
            { messageList = messageList }
            {}


messageString : View.Config -> String -> List String -> String
messageString vc key values =
    values
        |> List.map (Locale.string vc.locale)
        |> Locale.interpolated vc.locale (firstToUpper key)


logToText : View.Config -> List ( String, Int ) -> ( String, List String, Maybe Http.Error ) -> String
logToText vc lastBlocks ( key, values, error ) =
    let
        baseText =
            messageString vc key values
    in
    case error of
        Nothing ->
            baseText

        Just e ->
            baseText
                ++ ": "
                ++ (case e of
                        Http.BadStatus 404 ->
                            if key == loadingAddressKey then
                                List.Extra.getAt 1 values
                                    |> Maybe.andThen
                                        (\curr ->
                                            lastBlocks
                                                |> List.Extra.find (first >> String.toUpper >> (==) (String.toUpper curr))
                                                |> Maybe.map (second >> Locale.int vc.locale)
                                        )
                                    |> Maybe.map
                                        (List.singleton
                                            >> Locale.interpolated vc.locale "Statusbar-not-found-info"
                                        )
                                    |> Maybe.withDefault (Locale.string vc.locale "not found")

                            else
                                Locale.httpErrorToString vc.locale e

                        Http.BadStatus 504 ->
                            Locale.httpErrorToString vc.locale e
                                ++ (if key == searchNeighborsKey then
                                        ". " ++ Locale.string vc.locale "Please try again with a lower depth/breadth setting."

                                    else
                                        ""
                                   )

                        Http.BadBody str ->
                            if str == Api.noExternalTransactions then
                                Locale.string vc.locale str

                            else
                                Locale.httpErrorToString vc.locale e

                        _ ->
                            Locale.httpErrorToString vc.locale e
                   )
