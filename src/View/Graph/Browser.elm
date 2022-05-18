module View.Graph.Browser exposing (browser)

import Api.Data
import Config.Graph as Graph
import Config.View as View
import Css as CssStyled
import Css.Browser as Css
import Dict
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Maybe.Extra
import Model.Graph.Address exposing (..)
import Model.Graph.Browser as Browser exposing (..)
import Model.Graph.Entity exposing (Entity)
import Msg.Graph exposing (Msg(..))
import Time
import Util.View exposing (none, toCssColor)
import View.Locale as Locale


type Value
    = String String
    | EntityId Entity
    | Transactions { noIncomingTxs : Int, noOutgoingTxs : Int }
    | Usage Time.Posix Int
    | Duration Int
    | Value String Api.Data.Values


type Row
    = Row ( String, Value )
    | Rule


browser : View.Config -> Graph.Config -> Browser.Model -> Html Msg
browser vc gc model =
    div
        [ Css.root vc |> css
        ]
        [ div
            [ Css.frame vc model.visible |> css
            ]
            (case model.type_ of
                Browser.None ->
                    []

                Browser.Address address ->
                    [ browseAddress vc gc model.now address
                    ]

                Browser.Entity entity ->
                    [ browseEntity vc gc model.now entity
                    ]
            )
        ]


browse : View.Config -> Graph.Config -> List Row -> Html Msg
browse vc gc rows =
    List.map (browseRow vc gc) rows
        |> div
            [ Css.propertyBoxTable vc |> css
            ]


browseRow : View.Config -> Graph.Config -> Row -> Html Msg
browseRow vc gc row =
    case row of
        Rule ->
            hr [ Css.propertyBoxRule vc |> css ] []

        Row ( key, value ) ->
            div
                [ Css.propertyBoxRow vc |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    [ Locale.text vc.locale key
                    ]
                , span
                    [ Css.propertyBoxValue vc |> css
                    ]
                    [ browseValue vc gc value
                    ]
                ]


browseValue : View.Config -> Graph.Config -> Value -> Html Msg
browseValue vc gc value =
    case value of
        String str ->
            text str

        EntityId entity ->
            div
                []
                [ entity.entity.tags
                    |> Maybe.andThen (.entityTags >> List.head)
                    |> Maybe.Extra.andThen2
                        (\cat tag ->
                            Dict.get cat gc.colors
                                |> Maybe.map
                                    (\color ->
                                        span
                                            [ css
                                                [ toCssColor color
                                                    |> CssStyled.color
                                                ]
                                            ]
                                            [ text tag.label
                                            ]
                                    )
                        )
                        entity.category
                    |> Maybe.withDefault none
                , span
                    [ Css.propertyBoxEntityId vc |> css
                    ]
                    [ String.fromInt entity.entity.entity
                        |> text
                    ]
                ]

        Transactions { noIncomingTxs, noOutgoingTxs } ->
            div
                []
                [ noIncomingTxs
                    + noOutgoingTxs
                    |> Locale.int vc.locale
                    |> text
                , span
                    [ Css.propertyBoxIncomingTxs vc |> css
                    ]
                    [ text " "
                    , FontAwesome.icon FontAwesome.longArrowAltDown |> Html.fromUnstyled
                    , " " ++ Locale.int vc.locale noIncomingTxs |> text
                    ]
                , span
                    [ Css.propertyBoxOutgoingTxs vc |> css
                    ]
                    [ text " "
                    , FontAwesome.icon FontAwesome.longArrowAltUp |> Html.fromUnstyled
                    , " " ++ Locale.int vc.locale noOutgoingTxs |> text
                    ]
                ]

        Usage now timestamp ->
            div
                []
                [ span
                    [ Css.propertyBoxUsageTimestamp vc |> css
                    ]
                    [ Locale.timestamp vc.locale timestamp |> text
                    ]
                , span
                    [ Css.propertyBoxUsageRelative vc |> css
                    ]
                    [ " ("
                        ++ Locale.relativeTime vc.locale now timestamp
                        ++ ")"
                        |> text
                    ]
                ]

        Duration dur ->
            span
                [ Css.propertyBoxActivityPeriod vc |> css
                ]
                [ 1000 * dur |> Locale.durationToString vc.locale |> text
                ]

        Value coinCode v ->
            span
                []
                [ Locale.currency vc.locale coinCode v
                    |> text
                ]


browseAddress : View.Config -> Graph.Config -> Time.Posix -> Address -> Html Msg
browseAddress vc gc now address =
    browse vc
        gc
        [ Row ( "Address", String address.address.address )
        , Row ( "Currency", address.address.currency |> String.toUpper |> String )
        , Row
            ( "Tags"
            , Maybe.map List.length address.address.tags
                |> Maybe.withDefault 0
                |> String.fromInt
                |> String
            )
        , Rule
        , Row
            ( "Transactions"
            , Transactions
                { noIncomingTxs = address.address.noIncomingTxs
                , noOutgoingTxs = address.address.noOutgoingTxs
                }
            )
        , Row
            ( "Receiving addresses"
            , Locale.int vc.locale address.address.outDegree |> String
            )
        , Row
            ( "Sending addresses"
            , Locale.int vc.locale address.address.inDegree |> String
            )
        , Rule
        , Row
            ( "First usage"
            , Usage now address.address.firstTx.timestamp
            )
        , Row
            ( "Last usage"
            , Usage now address.address.lastTx.timestamp
            )
        , Row
            ( "Activity period"
            , address.address.firstTx.timestamp
                - address.address.lastTx.timestamp
                |> Duration
            )
        , Rule
        , Row
            ( "Total received"
            , Value address.address.currency address.address.totalReceived
            )
        , Row
            ( "Final balance"
            , Value address.address.currency address.address.balance
            )
        ]


browseEntity : View.Config -> Graph.Config -> Time.Posix -> Entity -> Html Msg
browseEntity vc gc now entity =
    browse vc
        gc
        [ Row ( "Entity", EntityId entity )
        , Row ( "Root address", String entity.entity.rootAddress )
        , Row ( "Currency", entity.entity.currency |> String.toUpper |> String )
        , Row
            ( "Address Tags"
            , Maybe.map (.addressTags >> List.length) entity.entity.tags
                |> Maybe.withDefault 0
                |> String.fromInt
                |> String
            )
        , Rule
        , Row
            ( "Transactions"
            , Transactions
                { noIncomingTxs = entity.entity.noIncomingTxs
                , noOutgoingTxs = entity.entity.noOutgoingTxs
                }
            )
        , Row
            ( "Receiving entities"
            , Locale.int vc.locale entity.entity.outDegree |> String
            )
        , Row
            ( "Sending entities"
            , Locale.int vc.locale entity.entity.inDegree |> String
            )
        , Rule
        , Row
            ( "First usage"
            , Usage now entity.entity.firstTx.timestamp
            )
        , Row
            ( "Last usage"
            , Usage now entity.entity.lastTx.timestamp
            )
        , Row
            ( "Activity period"
            , entity.entity.firstTx.timestamp
                - entity.entity.lastTx.timestamp
                |> Duration
            )
        , Rule
        , Row
            ( "Total received"
            , Value entity.entity.currency entity.entity.totalReceived
            )
        , Row
            ( "Final balance"
            , Value entity.entity.currency entity.entity.balance
            )
        ]
