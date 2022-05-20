module View.Graph.Browser exposing (Row(..), TableLink, Value(..), browse, browser, elseLoading, ifLoaded)

import Api.Data
import Config.Graph as Graph
import Config.View as View
import Css as CssStyled
import Css.Browser as Css
import Css.View as CssView
import Dict
import FontAwesome
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Encode
import Maybe.Extra
import Model.Graph.Address exposing (..)
import Model.Graph.Browser as Browser exposing (..)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Table exposing (..)
import Msg.Graph exposing (Msg(..))
import Plugin exposing (Plugins)
import Plugin.Model exposing (PluginStates)
import Plugin.View.Graph.Browser
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Time
import Util.View exposing (none, toCssColor)
import View.Graph.Table as Table
import View.Graph.Table.AddressTxsTable as AddressTxsTable
import View.Locale as Locale


type Value msg
    = String String
    | EntityId Graph.Config Entity
    | Transactions { noIncomingTxs : Int, noOutgoingTxs : Int }
    | Usage Time.Posix Int
    | Duration Int
    | Value String Api.Data.Values
    | Input (String -> msg) String
    | LoadingValue


type alias TableLink =
    { title : String
    , link : String
    , active : Bool
    }


type Row id a msg
    = Row ( String, Loadable id a -> Value msg, Maybe TableLink )
    | Rule


browser : Plugins -> View.Config -> Graph.Config -> PluginStates -> Browser.Model -> Html Msg
browser plugins vc gc states model =
    div
        [ Css.root vc |> css
        ]
        [ div
            [ Css.frame vc model.visible |> css
            ]
            (case model.type_ of
                Browser.None ->
                    []

                Browser.Address loadable table ->
                    browseAddress vc gc model.now loadable
                        :: (table
                                |> Maybe.map (browseAddressTable vc gc (loadableAddressCurrency loadable))
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []
                           )

                Browser.Entity loadable table ->
                    browseEntity vc gc model.now loadable
                        :: (table
                                |> Maybe.map (browseEntityTable vc gc (loadableEntityCurrency loadable))
                                |> Maybe.map List.singleton
                                |> Maybe.withDefault []
                           )

                Browser.Plugin pid ->
                    browsePlugin plugins vc pid states
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            )
        ]


browse : View.Config -> Loadable id a -> List (Row id a msg) -> Html msg
browse vc a rows =
    List.map (browseRow vc a) rows
        |> div
            [ Css.propertyBoxTable vc |> css
            ]


browseRow : View.Config -> Loadable id a -> Row id a msg -> Html msg
browseRow vc thing row =
    case row of
        Rule ->
            hr [ Css.propertyBoxRule vc |> css ] []

        Row ( key, toValue, table ) ->
            div
                [ Css.propertyBoxRow vc |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    [ Locale.text vc.locale key
                    ]
                , toValue thing |> valueCell vc
                , table
                    |> Maybe.map (tableLink vc)
                    |> Maybe.withDefault none
                ]


valueCell : View.Config -> Value msg -> Html msg
valueCell vc value =
    span
        [ Css.propertyBoxValue vc |> css
        ]
        [ browseValue vc value
        ]


tableLink : View.Config -> TableLink -> Html msg
tableLink vc link =
    a
        [ Css.propertyBoxTableLink vc link.active |> css
        , href link.link
        , title link.title
        ]
        [ FontAwesome.icon FontAwesome.ellipsisH
            |> Html.fromUnstyled
        ]


browseValue : View.Config -> Value msg -> Html msg
browseValue vc value =
    case value of
        String str ->
            text str

        Input msg current ->
            input
                [ Html.Styled.Attributes.value current
                , onInput msg
                , CssView.input vc |> css
                ]
                []

        EntityId gc entity ->
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

        LoadingValue ->
            text "loading"


browseAddress : View.Config -> Graph.Config -> Time.Posix -> Loadable String Address -> Html Msg
browseAddress vc gc now address =
    let
        mkTableLink title tableTag =
            address
                |> makeTableLink
                    (.address >> .currency)
                    (.address >> .address)
                    (\currency id ->
                        { title = Locale.string vc.locale title
                        , link =
                            Route.addressRoute
                                { currency = currency
                                , address = id
                                , table = Just tableTag
                                , layer = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = False
                        }
                    )
    in
    browse vc
        address
        [ Row
            ( "Address"
            , ifLoaded (.address >> .address >> String)
                >> elseShowAddress
            , Nothing
            )
        , Row
            ( "Currency"
            , ifLoaded (.address >> .currency >> String.toUpper >> String)
                >> elseShowCurrency
            , Nothing
            )
        , Row
            ( "Tags"
            , ifLoaded
                (\a ->
                    Maybe.map List.length a.address.tags
                        |> Maybe.withDefault 0
                        |> String.fromInt
                        |> String
                )
                >> elseLoading
            , mkTableLink "List address tags" Route.AddressTagsTable
            )
        , Rule
        , Row
            ( "Transactions"
            , ifLoaded
                (\a ->
                    Transactions
                        { noIncomingTxs = a.address.noIncomingTxs
                        , noOutgoingTxs = a.address.noOutgoingTxs
                        }
                )
                >> elseLoading
            , mkTableLink "List address transactions" Route.AddressTxsTable
            )
        , Row
            ( "Receiving addresses"
            , ifLoaded (.address >> .outDegree >> Locale.int vc.locale >> String)
                >> elseLoading
            , mkTableLink "List receiving addresses" Route.AddressIncomingNeighborsTable
            )
        , Row
            ( "Sending addresses"
            , ifLoaded (.address >> .inDegree >> Locale.int vc.locale >> String)
                >> elseLoading
            , mkTableLink "List receiving addresses" Route.AddressOutgoingNeighborsTable
            )
        , Rule
        , Row
            ( "First usage"
            , ifLoaded (.address >> .firstTx >> .timestamp >> Usage now)
                >> elseLoading
            , Nothing
            )
        , Row
            ( "Last usage"
            , ifLoaded (.address >> .lastTx >> .timestamp >> Usage now)
                >> elseLoading
            , Nothing
            )
        , Row
            ( "Activity period"
            , ifLoaded
                (\a ->
                    a.address.firstTx.timestamp
                        - a.address.lastTx.timestamp
                        |> Duration
                )
                >> elseLoading
            , Nothing
            )
        , Rule
        , Row
            ( "Total received"
            , ifLoaded (\a -> Value a.address.currency a.address.totalReceived)
                >> elseLoading
            , Nothing
            )
        , Row
            ( "Final balance"
            , ifLoaded (\a -> Value a.address.currency a.address.balance)
                >> elseLoading
            , Nothing
            )
        ]


makeTableLink : (a -> String) -> (a -> id) -> (String -> id -> TableLink) -> Loadable id a -> Maybe TableLink
makeTableLink getCurrency getId make l =
    case l of
        Loading curr id ->
            make curr id
                |> Just

        Loaded a ->
            make (getCurrency a) (getId a)
                |> Just


ifLoaded : (a -> Value msg) -> Loadable id a -> Loadable id (Value msg)
ifLoaded toValue l =
    case l of
        Loading currency id ->
            Loading currency id

        Loaded a ->
            toValue a |> Loaded


elseLoading : Loadable id (Value msg) -> Value msg
elseLoading l =
    case l of
        Loading _ _ ->
            LoadingValue

        Loaded v ->
            v


elseShowAddress : Loadable String (Value msg) -> Value msg
elseShowAddress l =
    case l of
        Loading _ id ->
            String id

        Loaded v ->
            v


elseShowCurrency : Loadable id (Value msg) -> Value msg
elseShowCurrency l =
    case l of
        Loading currency _ ->
            String <| String.toUpper currency

        Loaded v ->
            v


browseEntity : View.Config -> Graph.Config -> Time.Posix -> Loadable Int Entity -> Html Msg
browseEntity vc gc now ent =
    let
        mkTableLink title tableTag =
            ent
                |> makeTableLink
                    (.entity >> .currency)
                    (.entity >> .entity)
                    (\currency id ->
                        { title = Locale.string vc.locale title
                        , link =
                            Route.entityRoute
                                { currency = currency
                                , entity = id
                                , table = Just tableTag
                                , layer = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = False
                        }
                    )
    in
    browse vc
        ent
        [ Row ( "Entity", ifLoaded (EntityId gc) >> elseLoading, Nothing )
        , Row
            ( "Root address"
            , ifLoaded (.entity >> .rootAddress >> String) >> elseLoading
            , Nothing
            )
        , Row
            ( "Currency"
            , ifLoaded (.entity >> .currency >> String.toUpper >> String) >> elseShowCurrency
            , Nothing
            )
        , Row
            ( "Addresses"
            , ifLoaded
                (\entity ->
                    Locale.int vc.locale entity.entity.noAddresses
                        |> String
                )
                >> elseLoading
            , mkTableLink "List addresses" Route.EntityAddressesTable
            )
        , Row
            ( "Address Tags"
            , ifLoaded
                (\entity ->
                    Maybe.map (.addressTags >> List.length) entity.entity.tags
                        |> Maybe.withDefault 0
                        |> String.fromInt
                        |> String
                )
                >> elseLoading
            , mkTableLink "List address tags" Route.EntityTagsTable
            )
        , Rule
        , Row
            ( "Transactions"
            , ifLoaded
                (\entity ->
                    Transactions
                        { noIncomingTxs = entity.entity.noIncomingTxs
                        , noOutgoingTxs = entity.entity.noOutgoingTxs
                        }
                )
                >> elseLoading
            , mkTableLink "List entity transactions" Route.EntityTxsTable
            )
        , Row
            ( "Receiving entities"
            , ifLoaded
                (\entity ->
                    Locale.int vc.locale entity.entity.outDegree
                        |> String
                )
                >> elseLoading
            , mkTableLink "List receiving entities" Route.EntityIncomingNeighborsTable
            )
        , Row
            ( "Sending entities"
            , ifLoaded (\entity -> Locale.int vc.locale entity.entity.inDegree |> String)
                >> elseLoading
            , mkTableLink "List sending entities" Route.EntityOutgoingNeighborsTable
            )
        , Rule
        , Row
            ( "First usage"
            , ifLoaded (\entity -> Usage now entity.entity.firstTx.timestamp) >> elseLoading
            , Nothing
            )
        , Row
            ( "Last usage"
            , ifLoaded (\entity -> Usage now entity.entity.lastTx.timestamp) >> elseLoading
            , Nothing
            )
        , Row
            ( "Activity period"
            , ifLoaded (\entity -> entity.entity.firstTx.timestamp - entity.entity.lastTx.timestamp |> Duration)
                >> elseLoading
            , Nothing
            )
        , Rule
        , Row
            ( "Total received"
            , ifLoaded (\entity -> Value entity.entity.currency entity.entity.totalReceived)
                >> elseLoading
            , Nothing
            )
        , Row
            ( "Final balance"
            , ifLoaded (\entity -> Value entity.entity.currency entity.entity.balance)
                >> elseLoading
            , Nothing
            )
        ]


browseAddressTable : View.Config -> Graph.Config -> String -> AddressTable -> Html Msg
browseAddressTable vc gc coinCode table =
    case table of
        AddressTxsTable t ->
            Table.table vc (AddressTxsTable.config vc coinCode) t.state t.data

        _ ->
            none


browseEntityTable : View.Config -> Graph.Config -> String -> EntityTable -> Html Msg
browseEntityTable vc gc coinCode table =
    Debug.todo "browseEntityTable"



{-
   case table of
       EntityTxsTable t ->
           Table.view (AddressTxsTable.config vc coinCode) t.state t.data

       _ ->
           none
-}


browsePlugin : Plugins -> View.Config -> String -> PluginStates -> Maybe (Html Msg)
browsePlugin plugins vc pid states =
    Plugin.View.Graph.Browser.propertyBox plugins vc pid states
