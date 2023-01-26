module View.Graph.Browser exposing (browseRow, browseValue, browser, elseLoading, frame, ifLoaded, propertyBox, rule)

--import Plugin.View.Graph.Address
--import Plugin.View.Graph.Browser
--import Plugin.View.Graph.Entity

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
import Init.Graph.Id as Id
import Json.Decode as JD
import Json.Encode
import List.Extra
import Maybe.Extra
import Model.Address as A
import Model.Currency as Currency
import Model.Entity as E
import Model.Graph.Address exposing (..)
import Model.Graph.Browser as Browser exposing (..)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer
import Model.Graph.Link as Link exposing (Link)
import Model.Graph.Table exposing (..)
import Model.Graph.Tag as Tag
import Model.Locale as Locale
import Msg.Graph exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View exposing (Plugins)
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Time
import Tuple exposing (..)
import Util.Graph
import Util.View exposing (none, toCssColor)
import View.Graph.Table as Table
import View.Graph.Table.AddressNeighborsTable as AddressNeighborsTable
import View.Graph.Table.AddressTagsTable as AddressTagsTable
import View.Graph.Table.AddressTxsUtxoTable as AddressTxsUtxoTable
import View.Graph.Table.AddresslinkTxsUtxoTable as AddresslinkTxsUtxoTable
import View.Graph.Table.EntityAddressesTable as EntityAddressesTable
import View.Graph.Table.EntityNeighborsTable as EntityNeighborsTable
import View.Graph.Table.LabelAddressTagsTable as LabelAddressTagsTable
import View.Graph.Table.TxUtxoTable as TxUtxoTable
import View.Graph.Table.TxsAccountTable as TxsAccountTable
import View.Graph.Table.TxsUtxoTable as TxsUtxoTable
import View.Graph.Table.UserAddressTagsTable as UserAddressTagsTable
import View.Locale as Locale


cm : Maybe Msg
cm =
    Just UserClicksDownloadCSVInTable


frame : View.Config -> Bool -> List (Html msg) -> Html msg
frame vc visible =
    let
        width =
            vc.size
                |> Maybe.map .width
                |> Maybe.withDefault 0
    in
    div
        [ Css.frame vc visible
            |> css
        ]
        >> List.singleton
        >> div [ Css.root vc width |> css ]


browser : Plugins -> ModelState -> View.Config -> Graph.Config -> Browser.Model -> Html Msg
browser plugins states vc gc model =
    frame vc
        model.visible
        (case model.type_ of
            Browser.None ->
                []

            Browser.Address loadable table ->
                browseAddress plugins states vc model.now loadable
                    :: (table
                            |> Maybe.map
                                (\t ->
                                    let
                                        neighborLayerHasAddress aid isOutgoing address =
                                            Layer.getAddress
                                                (Id.initAddressId
                                                    { currency = address.currency
                                                    , id = address.address
                                                    , layer =
                                                        Id.layer aid
                                                            + (if isOutgoing then
                                                                1

                                                               else
                                                                -1
                                                              )
                                                    }
                                                )
                                                model.layers
                                                |> Maybe.Extra.isJust
                                    in
                                    browseAddressTable vc gc model.height neighborLayerHasAddress loadable t
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Entity loadable table ->
                browseEntity plugins states vc gc model.now loadable
                    :: (table
                            |> Maybe.map
                                (\t ->
                                    let
                                        entityHasAddress entityId address =
                                            Layer.getAddress
                                                (Id.initAddressId
                                                    { currency = address.currency
                                                    , id = address.address
                                                    , layer = Id.layer entityId
                                                    }
                                                )
                                                model.layers
                                                |> Maybe.Extra.isJust

                                        neighborLayerHasEntity eid isOutgoing entity =
                                            Layer.getEntity
                                                (Id.initEntityId
                                                    { currency = entity.currency
                                                    , id = entity.entity
                                                    , layer =
                                                        Id.layer eid
                                                            + (if isOutgoing then
                                                                1

                                                               else
                                                                -1
                                                              )
                                                    }
                                                )
                                                model.layers
                                                |> Maybe.Extra.isJust
                                    in
                                    browseEntityTable vc gc model.height entityHasAddress neighborLayerHasEntity loadable t
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Block loadable table ->
                browseBlock plugins states vc gc model.now loadable
                    :: (table
                            |> Maybe.map (browseBlockTable vc gc model.height loadable)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.TxUtxo loadable table ->
                browseTxUtxo plugins states vc gc model.now loadable
                    :: (table
                            |> Maybe.map (browseTxUtxoTable vc gc model.height loadable)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.TxAccount loadable accountCurrency table ->
                browseTxAccount plugins states vc gc model.now loadable table accountCurrency
                    :: (table
                            |> Maybe.map (browseTxAccountTable vc gc model.height loadable)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Addresslink source link table ->
                let
                    currency =
                        Id.currency source.id
                in
                browseAddresslink plugins states vc source link
                    :: (table
                            |> Maybe.map (browseAddresslinkTable vc gc model.height currency)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Entitylink source link table ->
                let
                    currency =
                        Id.currency source.id
                in
                browseEntitylink plugins states vc source link
                    :: (table
                            |> Maybe.map (browseAddresslinkTable vc gc model.height currency)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Label label table ->
                table
                    |> table_ vc Nothing model.height (LabelAddressTagsTable.config vc)
                    |> List.singleton

            Browser.UserTags table ->
                table
                    |> table_ vc cm model.height (UserAddressTagsTable.config vc gc)
                    |> List.singleton

            Browser.Plugin ->
                browsePlugin plugins vc states
        )


propertyBox : View.Config -> List (Html msg) -> Html msg
propertyBox vc =
    div
        [ Css.propertyBoxTable vc |> css
        , id "propertyBox"
        ]
        >> List.singleton
        >> div [ Css.propertyBoxRoot vc |> css ]


rule : View.Config -> Html msg
rule vc =
    hr [ Css.propertyBoxRule vc |> css ] []


browseRow : View.Config -> (r -> Html msg) -> Row r -> Html msg
browseRow vc map row =
    case row of
        Rule ->
            rule vc

        Note note ->
            div
                [ Css.propertyBoxRow vc |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    []
                , span
                    []
                    [ FontAwesome.exclamationTriangle
                        |> FontAwesome.icon
                        |> Html.fromUnstyled
                    , span
                        [ Css.propertyBoxNote vc |> css
                        ]
                        [ text note
                        ]
                    ]
                ]

        Row ( key, value, table ) ->
            div
                [ Css.propertyBoxRow vc |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    [ Locale.text vc.locale key
                    ]
                , span
                    []
                    [ div
                        [ Css.propertyBoxValueInner vc |> css
                        ]
                        [ map value
                        , table
                            |> Maybe.map (tableLink vc)
                            |> Maybe.withDefault none
                        ]
                    ]
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
            div [ css [ CssStyled.minHeight <| CssStyled.em 1 ] ]
                [ text str ]

        Html html ->
            html

        Input msg blur current ->
            input
                [ Html.Styled.Attributes.value current
                , onInput msg
                , onBlur blur
                , CssView.input vc |> css
                ]
                []

        EntityId gc entity ->
            div
                []
                [ entity.entity.bestAddressTag
                    |> Maybe.map
                        (\tag ->
                            span
                                [ tag.category
                                    |> Maybe.andThen (\cat -> Dict.get cat gc.colors)
                                    |> Maybe.map
                                        (toCssColor
                                            >> CssStyled.color
                                            >> List.singleton
                                        )
                                    |> Maybe.withDefault []
                                    |> css
                                ]
                                [ text
                                    (if String.isEmpty tag.label && not tag.tagpackIsPublic then
                                        Util.Graph.getCategory gc tag.category
                                            |> Maybe.withDefault (Locale.string vc.locale "Tag locked")

                                     else
                                        tag.label
                                    )
                                ]
                        )
                    |> Maybe.withDefault (span [] [ Locale.string vc.locale "Unknown" |> text ])
                , span
                    [ Css.propertyBoxEntityId vc |> css
                    ]
                    [ "("
                        ++ Locale.string vc.locale "ID"
                        ++ ": "
                        ++ String.fromInt entity.entity.entity
                        ++ ")"
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

        MultiValue parentCoin len values ->
            values
                |> List.map
                    (\( coinCode, v ) ->
                        tr []
                            [ String.toUpper coinCode
                                |> text
                                |> List.singleton
                                |> td [ Css.currencyCell vc |> css ]
                            , multiValue vc parentCoin coinCode v
                                |> text
                                |> List.singleton
                                |> td
                                    [ Css.valueCell vc
                                        ++ [ CssStyled.ex (toFloat len) |> CssStyled.width ]
                                        |> css
                                    ]
                            ]
                    )
                |> table
                    []

        LoadingValue ->
            Util.View.loadingSpinner vc Css.loadingSpinner


browseAddress : Plugins -> ModelState -> View.Config -> Time.Posix -> Loadable String Address -> Html Msg
browseAddress plugins states vc now address =
    (rowsAddress vc now address |> List.map (browseRow vc (browseValue vc)))
        ++ [ rule vc ]
        ++ (case address of
                Loading _ _ ->
                    []

                Loaded ad ->
                    Plugin.View.addressProperties plugins states ad.plugins vc
           )
        |> propertyBox vc


rowsAddress : View.Config -> Time.Posix -> Loadable String Address -> List (Row (Value Msg))
rowsAddress vc now address =
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

        rowsPart1 =
            [ Row
                ( "Tags"
                , address
                    |> ifLoaded
                        (\a ->
                            (Maybe.map List.length a.tags |> Maybe.withDefault 0)
                                + (Maybe.map (\_ -> 1) a.userTag |> Maybe.withDefault 0)
                                |> String.fromInt
                                |> String
                        )
                    |> elseLoading
                , mkTableLink "List address tags" Route.AddressTagsTable
                )
            , Rule
            , Row
                ( "Transactions"
                , address
                    |> ifLoaded
                        (\a ->
                            Transactions
                                { noIncomingTxs = a.address.noIncomingTxs
                                , noOutgoingTxs = a.address.noOutgoingTxs
                                }
                        )
                    |> elseLoading
                , mkTableLink "List address transactions" Route.AddressTxsTable
                )
            , Row
                ( "Receiving addresses"
                , address
                    |> ifLoaded (.address >> .outDegree >> Locale.int vc.locale >> String)
                    |> elseLoading
                , mkTableLink "List receiving addresses" Route.AddressOutgoingNeighborsTable
                )
            , Row
                ( "Sending addresses"
                , address
                    |> ifLoaded (.address >> .inDegree >> Locale.int vc.locale >> String)
                    |> elseLoading
                , mkTableLink "List sending addresses" Route.AddressIncomingNeighborsTable
                )
            ]

        rowsPart2 =
            [ Row
                ( "Last usage"
                , address
                    |> ifLoaded (.address >> .lastTx >> .timestamp >> Usage now)
                    |> elseLoading
                , Nothing
                )
            , Row
                ( "Activity period"
                , address
                    |> ifLoaded
                        (\a ->
                            a.address.firstTx.timestamp
                                - a.address.lastTx.timestamp
                                |> Duration
                        )
                    |> elseLoading
                , Nothing
                )
            , Rule
            , Row
                ( "Total received"
                , address
                    |> ifLoaded
                        (totalReceivedValues .address
                            >> MultiValue (loadableAddressCurrency address) len
                        )
                    |> elseLoading
                , Nothing
                )
            , Row
                ( "Final balance"
                , address
                    |> ifLoaded
                        (balanceValues .address
                            >> MultiValue (loadableAddressCurrency address) len
                        )
                    |> elseLoading
                , Nothing
                )
            ]

        dataPart1 =
            case address of
                Loaded a ->
                    if a.address.status == Api.Data.AddressStatusNew then
                        []

                    else
                        rowsPart1

                Loading _ _ ->
                    rowsPart1

        dataPart2 =
            case address of
                Loaded a ->
                    if a.address.status == Api.Data.AddressStatusNew then
                        []

                    else
                        rowsPart2

                Loading _ _ ->
                    rowsPart2

        statusNote =
            case address of
                Loaded a ->
                    case a.address.status of
                        Api.Data.AddressStatusNew ->
                            [ Rule
                            , Locale.string vc.locale "Address statistics not yet computed"
                                |> Note
                            ]

                        Api.Data.AddressStatusDirty ->
                            []

                        Api.Data.AddressStatusClean ->
                            []

                Loading _ _ ->
                    []

        len =
            multiValueMaxLen vc .address address
    in
    [ Row
        ( "Address"
        , address
            |> ifLoaded (.address >> .address >> String)
            |> elseShowAddress
        , Nothing
        )
    , Row
        ( "Currency"
        , address
            |> ifLoaded (.address >> .currency >> String.toUpper >> String)
            |> elseShowCurrency
        , Nothing
        )
    ]
        ++ (if loadableAddress address |> .currency |> (==) "eth" then
                [ Row
                    ( "Smart contract"
                    , address
                        |> ifLoaded
                            (\a ->
                                String <|
                                    if a.address.isContract == Just True then
                                        Locale.string vc.locale "yes"

                                    else
                                        Locale.string vc.locale "no"
                            )
                        |> elseLoading
                    , Nothing
                    )
                ]

            else
                []
           )
        ++ dataPart1
        ++ [ Rule
           , Row
                ( "First usage"
                , address
                    |> ifLoaded (.address >> .firstTx >> .timestamp >> Usage now)
                    |> elseLoading
                , Nothing
                )
           ]
        ++ dataPart2
        ++ statusNote


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


elseShowTxAccount : Loadable ( String, Maybe Int ) (Value msg) -> Value msg
elseShowTxAccount l =
    case l of
        Loading _ ( id, _ ) ->
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


browseEntity : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable Int Entity -> Html Msg
browseEntity plugins states vc gc now entity =
    (rowsEntity vc gc now entity |> List.map (browseRow vc (browseValue vc)))
        ++ [ rule vc ]
        ++ (case entity of
                Loading _ _ ->
                    []

                Loaded en ->
                    Plugin.View.entityProperties plugins states en.plugins vc
           )
        |> propertyBox vc


browseBlock : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable Int Api.Data.Block -> Html Msg
browseBlock plugins states vc gc now block =
    (rowsBlock vc gc now block |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


browseTxUtxo : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable String Api.Data.TxUtxo -> Html Msg
browseTxUtxo plugins states vc gc now tx =
    (rowsTxUtxo vc gc now tx |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


browseTxAccount : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable ( String, Maybe Int ) Api.Data.TxAccount -> Maybe TxAccountTable -> String -> Html Msg
browseTxAccount plugins states vc gc now tx table coinCode =
    (rowsTxAccount vc gc now tx table coinCode |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


rowsEntity : View.Config -> Graph.Config -> Time.Posix -> Loadable Int Entity -> List (Row (Value Msg))
rowsEntity vc gc now ent =
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

        len =
            multiValueMaxLen vc .entity ent
    in
    [ Row ( "Entity", ent |> ifLoaded (EntityId gc) |> elseLoading, Nothing )
    , Row
        ( "Root address"
        , ent |> ifLoaded (.entity >> .rootAddress >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "Currency"
        , ent |> ifLoaded (.entity >> .currency >> String.toUpper >> String) |> elseShowCurrency
        , Nothing
        )
    , Row
        ( "Addresses"
        , ent
            |> ifLoaded
                (\entity ->
                    Locale.int vc.locale entity.entity.noAddresses
                        |> String
                )
            |> elseLoading
        , mkTableLink "List addresses" Route.EntityAddressesTable
        )
    , Row
        ( "Address tags"
        , ent
            |> ifLoaded
                (\e ->
                    (e.entity
                        |> .noAddressTags
                    )
                        + (Maybe.map (\_ -> 1) e.userTag |> Maybe.withDefault 0)
                        |> String.fromInt
                        |> String
                )
            |> elseLoading
        , mkTableLink "List address tags" Route.EntityTagsTable
        )
    , Rule
    , Row
        ( "Transactions"
        , ent
            |> ifLoaded
                (\entity ->
                    Transactions
                        { noIncomingTxs = entity.entity.noIncomingTxs
                        , noOutgoingTxs = entity.entity.noOutgoingTxs
                        }
                )
            |> elseLoading
        , mkTableLink "List entity transactions" Route.EntityTxsTable
        )
    , Row
        ( "Receiving entities"
        , ent
            |> ifLoaded
                (\entity ->
                    Locale.int vc.locale entity.entity.outDegree
                        |> String
                )
            |> elseLoading
        , mkTableLink "List receiving entities" Route.EntityOutgoingNeighborsTable
        )
    , Row
        ( "Sending entities"
        , ent
            |> ifLoaded (\entity -> Locale.int vc.locale entity.entity.inDegree |> String)
            |> elseLoading
        , mkTableLink "List sending entities" Route.EntityIncomingNeighborsTable
        )
    , Rule
    , Row
        ( "First usage"
        , ent |> ifLoaded (\entity -> Usage now entity.entity.firstTx.timestamp) |> elseLoading
        , Nothing
        )
    , Row
        ( "Last usage"
        , ent |> ifLoaded (\entity -> Usage now entity.entity.lastTx.timestamp) |> elseLoading
        , Nothing
        )
    , Row
        ( "Activity period"
        , ent
            |> ifLoaded (\entity -> entity.entity.firstTx.timestamp - entity.entity.lastTx.timestamp |> Duration)
            |> elseLoading
        , Nothing
        )
    , Rule
    , Row
        ( "Total received"
        , ent
            |> ifLoaded
                (totalReceivedValues .entity
                    >> MultiValue (loadableEntityCurrency ent) len
                )
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Final balance"
        , ent
            |> ifLoaded
                (balanceValues .entity
                    >> MultiValue (loadableEntityCurrency ent) len
                )
            |> elseLoading
        , Nothing
        )
    ]


rowsBlock : View.Config -> Graph.Config -> Time.Posix -> Loadable Int Api.Data.Block -> List (Row (Value Msg))
rowsBlock vc gc now block =
    let
        mkTableLink title tableTag =
            block
                |> makeTableLink
                    .currency
                    .height
                    (\currency id ->
                        { title = Locale.string vc.locale title
                        , link =
                            Route.blockRoute
                                { currency = currency
                                , block = id
                                , table = Just tableTag
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = False
                        }
                    )
    in
    [ Row ( "Height", block |> ifLoaded (.height >> Locale.int vc.locale >> String) |> elseLoading, Nothing )
    , Row
        ( "Currency"
        , block
            |> ifLoaded (.currency >> String.toUpper >> String)
            |> elseShowCurrency
        , Nothing
        )
    , Row
        ( "Transactions"
        , block
            |> ifLoaded (.noTxs >> Locale.int vc.locale >> String)
            |> elseLoading
        , mkTableLink "List block transactions" Route.BlockTxsTable
        )
    , Row
        ( "Block hash"
        , block
            |> ifLoaded (.blockHash >> String)
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Created"
        , block |> ifLoaded (.timestamp >> Locale.timestamp vc.locale >> String) |> elseLoading
        , Nothing
        )
    ]


browseAddressTable : View.Config -> Graph.Config -> Maybe Float -> (Id.AddressId -> Bool -> A.Address -> Bool) -> Loadable String Address -> AddressTable -> Html Msg
browseAddressTable vc gc height neighborLayerHasAddress address table =
    let
        ( coinCode, addressId ) =
            case address of
                Loaded a ->
                    ( a.address.currency, a.id |> Just )

                Loading curr _ ->
                    ( curr, Nothing )

        tt =
            table_ vc cm height
    in
    case table of
        AddressTxsUtxoTable t ->
            tt (AddressTxsUtxoTable.config vc coinCode) t

        AddressTxsAccountTable t ->
            tt (TxsAccountTable.config vc coinCode) t

        AddressTagsTable t ->
            table_ vc Nothing height (AddressTagsTable.config vc gc Nothing Nothing (\_ _ -> False)) t

        AddressIncomingNeighborsTable t ->
            tt (AddressNeighborsTable.config vc False coinCode addressId neighborLayerHasAddress) t

        AddressOutgoingNeighborsTable t ->
            tt (AddressNeighborsTable.config vc True coinCode addressId neighborLayerHasAddress) t


table_ : View.Config -> Maybe Msg -> Maybe Float -> Table.Config data Msg -> Table data -> Html Msg
table_ vc csvMsg =
    Table.table vc
        [ stopPropagationOn "scroll" (JD.map (\pos -> ( UserScrolledTable pos, True )) decodeScrollPos)
        ]
        { filter = Just UserInputsFilterTable
        , csv = csvMsg
        }


decodeScrollPos : JD.Decoder ScrollPos
decodeScrollPos =
    JD.map3 ScrollPos
        (JD.oneOf [ JD.at [ "target", "scrollTop" ] JD.float, JD.at [ "target", "scrollingElement", "scrollTop" ] JD.float ])
        (JD.oneOf [ JD.at [ "target", "scrollHeight" ] JD.int, JD.at [ "target", "scrollingElement", "scrollHeight" ] JD.int ])
        (JD.map2 Basics.max offsetHeight clientHeight)


offsetHeight : JD.Decoder Int
offsetHeight =
    JD.oneOf [ JD.at [ "target", "offsetHeight" ] JD.int, JD.at [ "target", "scrollingElement", "offsetHeight" ] JD.int ]


clientHeight : JD.Decoder Int
clientHeight =
    JD.oneOf [ JD.at [ "target", "clientHeight" ] JD.int, JD.at [ "target", "scrollingElement", "clientHeight" ] JD.int ]


browseEntityTable : View.Config -> Graph.Config -> Maybe Float -> (Id.EntityId -> A.Address -> Bool) -> (Id.EntityId -> Bool -> E.Entity -> Bool) -> Loadable Int Entity -> EntityTable -> Html Msg
browseEntityTable vc gc height entityHasAddress neighborLayerHasEntity entity table =
    let
        ( coinCode, entityId, bestAddressTag ) =
            case entity of
                Loaded e ->
                    ( e.entity.currency, e.id |> Just, e.entity.bestAddressTag )

                Loading curr _ ->
                    ( curr, Nothing, Nothing )

        tt =
            table_ vc cm height
    in
    case table of
        EntityAddressesTable t ->
            tt (EntityAddressesTable.config vc coinCode entityId entityHasAddress) t

        EntityTxsUtxoTable t ->
            tt (AddressTxsUtxoTable.config vc coinCode) t

        EntityTxsAccountTable t ->
            tt (TxsAccountTable.config vc coinCode) t

        EntityTagsTable t ->
            table_ vc Nothing height (AddressTagsTable.config vc gc bestAddressTag entityId entityHasAddress) t

        EntityIncomingNeighborsTable t ->
            tt (EntityNeighborsTable.config vc False coinCode entityId neighborLayerHasEntity) t

        EntityOutgoingNeighborsTable t ->
            tt (EntityNeighborsTable.config vc True coinCode entityId neighborLayerHasEntity) t


browseBlockTable : View.Config -> Graph.Config -> Maybe Float -> Loadable Int Api.Data.Block -> BlockTable -> Html Msg
browseBlockTable vc gc height block table =
    let
        ( coinCode, blockId ) =
            case block of
                Loaded e ->
                    ( e.currency, e.height |> Just )

                Loading curr _ ->
                    ( curr, Nothing )
    in
    case table of
        BlockTxsUtxoTable t ->
            table_ vc cm height (TxsUtxoTable.config vc coinCode) t

        BlockTxsAccountTable t ->
            table_ vc cm height (TxsAccountTable.config vc coinCode) t


browseTxUtxoTable : View.Config -> Graph.Config -> Maybe Float -> Loadable String Api.Data.TxUtxo -> TxUtxoTable -> Html Msg
browseTxUtxoTable vc gc height tx table =
    let
        ( coinCode, txHash ) =
            case tx of
                Loaded e ->
                    ( e.currency, e.txHash |> Just )

                Loading curr _ ->
                    ( curr, Nothing )
    in
    case table of
        TxUtxoInputsTable t ->
            table_ vc cm height (TxUtxoTable.config vc False coinCode) t

        TxUtxoOutputsTable t ->
            table_ vc cm height (TxUtxoTable.config vc True coinCode) t


browseTxAccountTable : View.Config -> Graph.Config -> Maybe Float -> Loadable ( String, Maybe Int ) Api.Data.TxAccount -> TxAccountTable -> Html Msg
browseTxAccountTable vc gc height tx (TokenTxsTable table) =
    let
        ( coinCode, txHash ) =
            case tx of
                Loaded e ->
                    ( e.currency, e.txHash |> Just )

                Loading curr _ ->
                    ( curr, Nothing )
    in
    table_ vc cm height (TxsAccountTable.config vc coinCode) table


browsePlugin : Plugins -> View.Config -> ModelState -> List (Html Msg)
browsePlugin plugins vc states =
    Plugin.View.browser plugins vc states


rowsTxUtxo : View.Config -> Graph.Config -> Time.Posix -> Loadable String Api.Data.TxUtxo -> List (Row (Value Msg))
rowsTxUtxo vc gc now tx =
    let
        mkTableLink title tableTag =
            tx
                |> makeTableLink
                    .currency
                    .txHash
                    (\currency id ->
                        { title = Locale.string vc.locale title
                        , link =
                            Route.txRoute
                                { currency = currency
                                , txHash = id
                                , table = Just tableTag
                                , tokenTxId = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = False
                        }
                    )
    in
    [ Row
        ( "Transaction"
        , tx
            |> ifLoaded (.txHash >> String)
            |> elseShowAddress
        , Nothing
        )
    , Row
        ( "Included in block"
        , tx |> ifLoaded (.height >> Locale.int vc.locale >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "Created"
        , tx |> ifLoaded (.timestamp >> Locale.timestamp vc.locale >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "No. inputs"
        , tx
            |> ifLoaded
                (.noInputs
                    >> Locale.int vc.locale
                    >> String
                )
            |> elseLoading
        , mkTableLink "List sending addresses" Route.TxInputsTable
        )
    , Row
        ( "No. outputs"
        , tx
            |> ifLoaded
                (.noOutputs
                    >> Locale.int vc.locale
                    >> String
                )
            |> elseLoading
        , mkTableLink "List receiving addresses" Route.TxOutputsTable
        )
    , Row
        ( "total input"
        , tx
            |> ifLoaded
                (\t -> Value t.currency t.totalInput)
            |> elseLoading
        , Nothing
        )
    , Row
        ( "total output"
        , tx
            |> ifLoaded
                (\t -> Value t.currency t.totalOutput)
            |> elseLoading
        , Nothing
        )
    ]


rowsTxAccount : View.Config -> Graph.Config -> Time.Posix -> Loadable ( String, Maybe Int ) Api.Data.TxAccount -> Maybe TxAccountTable -> String -> List (Row (Value Msg))
rowsTxAccount vc gc now tx table coinCode =
    let
        txLink getAddress tx_ =
            a
                [ Route.addressRoute
                    { currency = coinCode
                    , address = getAddress tx_
                    , layer = Nothing
                    , table = Nothing
                    }
                    |> Route.graphRoute
                    |> toUrl
                    |> href
                , CssView.link vc |> css
                ]
                [ getAddress tx_ |> text
                ]
                |> Html

        mkTableLink title tableTag =
            tx
                |> makeTableLink
                    (\_ -> "eth")
                    (\d -> ( d.txHash, d.tokenTxId ))
                    (\currency id ->
                        { title = Locale.string vc.locale title
                        , link =
                            Route.txRoute
                                { currency = currency
                                , txHash = first id
                                , table = Just tableTag
                                , tokenTxId = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = False
                        }
                    )
    in
    [ Row
        ( "Transaction"
        , tx
            |> ifLoaded (.txHash >> String)
            |> elseShowTxAccount
        , Nothing
        )
    , Row
        ( "Value"
        , tx
            |> ifLoaded
                (\t -> Value t.currency t.value)
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Included in block"
        , tx |> ifLoaded (.height >> Locale.int vc.locale >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "Created"
        , tx |> ifLoaded (.timestamp >> Locale.timestamp vc.locale >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "Sending address"
        , tx
            |> ifLoaded (txLink .fromAddress)
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Receiving address"
        , tx
            |> ifLoaded (txLink .toAddress)
            |> elseLoading
        , Nothing
        )
    ]
        ++ (if loadableCurrency tx == "eth" then
                [ Row
                    ( "Token transactions"
                    , String <|
                        case table of
                            Just (TokenTxsTable t) ->
                                List.length t.data
                                    |> String.fromInt

                            _ ->
                                ""
                    , mkTableLink "Show token transactions" Route.TokenTxsTable
                    )
                ]

            else
                []
           )


browseAddresslink : Plugins -> ModelState -> View.Config -> Address -> Link Address -> Html Msg
browseAddresslink plugins states vc source link =
    (rowsAddresslink vc source link |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


rowsAddresslink : View.Config -> Address -> Link Address -> List (Row (Value Msg))
rowsAddresslink vc source link =
    let
        currency =
            Id.currency source.id

        linkData =
            case link.link of
                Link.LinkData ld ->
                    Just ( ld.value, ld.noTxs )

                Link.PlaceholderLinkData ->
                    Nothing
    in
    [ Row
        ( "Source"
        , source.id
            |> Id.addressId
            |> String
        , Nothing
        )
    , Row
        ( "Target"
        , link.node.id
            |> Id.addressId
            |> String
        , Nothing
        )
    , Row
        ( "Transactions"
        , linkData
            |> Maybe.map
                (second >> Locale.int vc.locale)
            |> Maybe.withDefault ""
            |> String
        , Just
            { title = Locale.string vc.locale "Transactions"
            , link =
                Route.addresslinkRoute
                    { currency = currency
                    , src = Id.addressId source.id
                    , srcLayer = Id.layer source.id
                    , dst = Id.addressId link.node.id
                    , dstLayer = Id.layer link.node.id
                    , table = Just Route.AddresslinkTxsTable
                    }
                    |> Route.graphRoute
                    |> toUrl
            , active = False
            }
        )
    , Row
        ( "Value"
        , linkData
            |> Maybe.map (first >> Value currency)
            |> Maybe.withDefault (String "")
        , Nothing
        )
    ]


browseEntitylink : Plugins -> ModelState -> View.Config -> Entity -> Link Entity -> Html Msg
browseEntitylink plugins states vc source link =
    (rowsEntitylink vc source link |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


rowsEntitylink : View.Config -> Entity -> Link Entity -> List (Row (Value Msg))
rowsEntitylink vc source link =
    let
        currency =
            Id.currency source.id

        linkData =
            case link.link of
                Link.LinkData ld ->
                    Just ( ld.value, ld.noTxs )

                Link.PlaceholderLinkData ->
                    Nothing
    in
    [ Row
        ( "Source"
        , source.id
            |> Id.entityId
            |> String.fromInt
            |> String
        , Nothing
        )
    , Row
        ( "Target"
        , link.node.id
            |> Id.entityId
            |> String.fromInt
            |> String
        , Nothing
        )
    , Row
        ( "Transactions"
        , linkData
            |> Maybe.map
                (second >> Locale.int vc.locale)
            |> Maybe.withDefault ""
            |> String
        , Just
            { title = Locale.string vc.locale "Transactions"
            , link =
                Route.entitylinkRoute
                    { currency = currency
                    , src = Id.entityId source.id
                    , srcLayer = Id.layer source.id
                    , dst = Id.entityId link.node.id
                    , dstLayer = Id.layer link.node.id
                    , table = Just Route.AddresslinkTxsTable
                    }
                    |> Route.graphRoute
                    |> toUrl
            , active = False
            }
        )
    , Row
        ( "Value"
        , linkData
            |> Maybe.map (first >> Value currency)
            |> Maybe.withDefault (String "")
        , Nothing
        )
    ]


browseAddresslinkTable : View.Config -> Graph.Config -> Maybe Float -> String -> AddresslinkTable -> Html Msg
browseAddresslinkTable vc gc height coinCode table =
    case table of
        AddresslinkTxsUtxoTable t ->
            table_ vc cm height (AddresslinkTxsUtxoTable.config vc coinCode) t

        AddresslinkTxsAccountTable t ->
            table_ vc cm height (TxsAccountTable.config vc coinCode) t


multiValue : View.Config -> String -> String -> Api.Data.Values -> String
multiValue vc parentCoin coinCode v =
    if parentCoin == "eth" && vc.locale.currency /= Currency.Coin then
        Locale.currency vc.locale coinCode v

    else
        Locale.currencyWithoutCode vc.locale coinCode v


type alias AddressOrEntity a =
    { a
        | balance : Api.Data.Values
        , totalReceived : Api.Data.Values
        , tokenBalances : Maybe (Dict.Dict String Api.Data.Values)
        , totalTokensReceived : Maybe (Dict.Dict String Api.Data.Values)
        , totalTokensSpent : Maybe (Dict.Dict String Api.Data.Values)
        , currency : String
    }


multiValueMaxLen : View.Config -> (thing -> AddressOrEntity a) -> Loadable comparable thing -> Int
multiValueMaxLen vc accessor thing =
    case thing of
        Loading _ _ ->
            0

        Loaded a ->
            totalReceivedValues accessor a
                ++ balanceValues accessor a
                |> List.map (\( currency, v ) -> multiValue vc (accessor a).currency currency v |> String.length)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 2


totalReceivedValues : (thing -> AddressOrEntity a) -> thing -> List ( String, Api.Data.Values )
totalReceivedValues accessor a =
    ( (accessor a).currency, (accessor a).totalReceived )
        :: ((accessor a).totalTokensReceived
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []
           )


balanceValues : (thing -> AddressOrEntity a) -> thing -> List ( String, Api.Data.Values )
balanceValues accessor a =
    ( (accessor a).currency, (accessor a).balance )
        :: ((accessor a).tokenBalances
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []
           )
