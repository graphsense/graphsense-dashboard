module View.Statusbar exposing (view)

import Api
import Config.View as View
import Css
import Css.Global
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
import Theme.Colors as Colors
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

        loading =
            not (List.isEmpty entries)

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
                        Theme.Html.Page.messageElementWithAttributes
                            (Theme.Html.Page.messageElementAttributes
                                |> Rs.s_root [ css [ fullWidthCss ] ]
                            )
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
                                Theme.Html.Page.messageElementWithAttributes
                                    (Theme.Html.Page.messageElementAttributes
                                        |> Rs.s_root [ css [ fullWidthCss ] ]
                                    )
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
        Theme.Html.Page.footerWithInstances
            (Theme.Html.Page.footerAttributes
                |> Rs.s_root
                    [ css
                        [ Css.Transitions.transition
                            [ Css.Transitions.minHeight 200
                            , Css.Transitions.maxHeight 200
                            ]
                        , Css.minHeight <| Css.px Theme.Html.Page.footer_details.height
                        , Css.maxHeight <| Css.px Theme.Html.Page.footer_details.height
                        , if loading then
                            fullWidthCss

                          else
                            -- shrink to the content instead of the generated 1920px
                            Css.width Css.auto |> Css.important
                        , Css.property "gap" "8px"
                        , Css.borderTopLeftRadius
                            (Css.px
                                (if loading then
                                    0

                                 else
                                    4
                                )
                            )
                        , Css.cursor Css.pointer
                        , Css.hover
                            [ Css.property "background-color" Colors.greyBlue50
                                |> Css.important
                            ]
                        ]
                    , onClick UserClickedStatusbar
                    ]
                |> Rs.s_messageElement
                    [ css
                        [ Css.width Css.auto |> Css.important
                        , Css.maxWidth (Css.vw 60)
                        , Css.minWidth (Css.px 0)
                        ]
                    ]
                |> Rs.s_message
                    [ css
                        [ Css.whiteSpace Css.noWrap |> Css.important
                        , Css.overflow Css.hidden |> Css.important
                        , Css.textOverflow Css.ellipsis
                        , Css.display Css.block |> Css.important
                        ]
                    ]
            )
            (Theme.Html.Page.footerInstances
                |> Rs.s_versionString (Just (versionWithChevron version))
            )
            { messageElement =
                { text = firstMessageText
                , iconVisible = loading
                , icon =
                    Loadingspinner.html
                        []
                }
            , root = { version = version }
            }

    else
        Theme.Html.Page.footerOpenWithAttributes
            { iconsCloseNoPadding =
                [ onClick UserClickedStatusbar
                , pointer
                , css
                    [ Css.flexShrink Css.zero
                    , Css.marginLeft (Css.px 8)
                    ]
                ]
            , messageList =
                [ css
                    [ Css.overflowY Css.auto |> Css.important

                    -- take the remaining width, so the scrollbar stays left
                    -- of the close button instead of running under it
                    , Css.width Css.auto |> Css.important
                    , Css.flexGrow (Css.num 1)
                    , Css.minWidth (Css.px 0)
                    , Css.maxHeight (Css.pct 100)
                    , Css.minHeight (Css.px 0)
                    ]
                ]
            , root =
                [ css
                    [ Css.Transitions.transition
                        [ Css.Transitions.minHeight 200
                        , Css.Transitions.maxHeight 200
                        ]
                    , Css.minHeight <| Css.px (Theme.Html.Page.footer_details.height * 10)
                    , Css.maxHeight <| Css.px (Theme.Html.Page.footer_details.height * 10)
                    , Css.property "border-top" ("1px solid " ++ Colors.greyBlue100)

                    -- a definite height lets the message list scroll inside it
                    , Css.height <| Css.px (Theme.Html.Page.footer_details.height * 10)
                    , fullWidthCss
                    ]
                ]
            , vector = []
            }
            { messageList = messageList }
            {}


{-| The version string plus an upward chevron, hinting that the collapsed
statusbar opens the message list when clicked.
-}
versionWithChevron : String -> Html Msg
versionWithChevron v =
    div
        [ css
            (Theme.Html.Page.footerVersionString_details.styles
                ++ [ Css.alignItems Css.center
                   , Css.property "gap" "2px"
                   ]
            )
        ]
        [ text v
        , div
            [ css
                [ Css.width (Css.px 12)
                , Css.height (Css.px 12)
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                , Css.transform (Css.rotate (Css.deg 180))
                , Css.Global.children
                    [ Css.Global.everything
                        [ Css.transform (Css.scale 0.5)
                        , Css.flexShrink Css.zero
                        ]
                    ]
                ]
            ]
            [ Icons.iconsChevronDownThin {} ]
        ]


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
