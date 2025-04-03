module View.Graph.Browser exposing (browseRow, browseValue, browser, frame, properties, propertyBox)

import Api.Data exposing (Entity)
import Basics.Extra exposing (uncurry)
import Config.Graph as Graph
import Config.View as View
import Css as CssStyled
import Css.Browser as Css
import Css.Table exposing (styles)
import Css.View as CssView
import Dict
import FontAwesome
import FontAwesome.Layers as FontAwesome
import Html.Attributes
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Init.Graph.Id as Id
import Json.Decode as JD
import List.Extra
import Maybe.Extra
import Model.Address as A
import Model.Currency as Currency exposing (asset, assetFromBase, tokensToValue)
import Model.Entity as E
import Model.Graph.Actor exposing (..)
import Model.Graph.Address exposing (..)
import Model.Graph.Browser as Browser exposing (..)
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Entity exposing (Entity)
import Model.Graph.Id as Id
import Model.Graph.Layer as Layer
import Model.Graph.Link as Link exposing (Link)
import Model.Graph.Table exposing (..)
import Model.Loadable as Loadable exposing (Loadable(..))
import Model.Locale as Locale
import Model.Node as Node
import Msg.Graph exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View exposing (Plugins)
import RecordSetter exposing (..)
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Time
import Tuple exposing (..)
import Util.Data as Data
import Util.ExternalLinks exposing (addProtocolPrefx, getFontAwesomeIconForUris)
import Util.Flags exposing (getFlagEmoji)
import Util.Graph exposing (filterTxValue)
import Util.View
    exposing
        ( copyableLongIdentifier
        , longIdentifier
        , none
        , toCssColor
        , truncateLongIdentifier
        )
import View.Button exposing (actorLink)
import View.Graph.Label as Label
import View.Graph.Table as Table
import View.Graph.Table.AddressNeighborsTable as AddressNeighborsTable
import View.Graph.Table.AddressTagsTable as AddressTagsTable
import View.Graph.Table.AddressTxsUtxoTable as AddressTxsUtxoTable
import View.Graph.Table.AddresslinkTxsUtxoTable as AddresslinkTxsUtxoTable
import View.Graph.Table.AllAssetsTable as AllAssetsTable
import View.Graph.Table.EntityAddressesTable as EntityAddressesTable
import View.Graph.Table.EntityNeighborsTable as EntityNeighborsTable
import View.Graph.Table.LabelAddressTagsTable as LabelAddressTagsTable
import View.Graph.Table.LinksTable as LinksTable
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
    List.intersperse (tableSeparator vc)
        >> div
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
                browseAddress plugins states vc gc model.now table loadable
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
                                    browseAddressTable vc gc neighborLayerHasAddress loadable t
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Entity loadable table ->
                browseEntity plugins states vc gc model.now table loadable
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
                                    browseEntityTable vc gc entityHasAddress neighborLayerHasEntity loadable t
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Actor loadable table ->
                browseActor plugins states vc gc model.now table loadable
                    :: (table
                            |> Maybe.map
                                (\t ->
                                    browseActorTable vc gc loadable t
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Block loadable table ->
                browseBlock plugins states vc gc model.now table loadable
                    :: (table
                            |> Maybe.map (browseBlockTable vc gc loadable)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.TxUtxo loadable table ->
                browseTxUtxo plugins states vc gc model.now table loadable
                    :: (table
                            |> Maybe.map (browseTxUtxoTable vc gc loadable)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.TxAccount loadable accountCurrency table ->
                browseTxAccount plugins states vc gc model.now loadable table accountCurrency
                    :: (table
                            |> Maybe.map (browseTxAccountTable vc gc loadable)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Addresslink source link table ->
                let
                    currency =
                        Id.currency source.id
                in
                browseAddresslink plugins states vc gc source table link
                    :: (table
                            |> Maybe.map (browseAddresslinkTable vc gc currency)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Entitylink source link table ->
                let
                    currency =
                        Id.currency source.id
                in
                browseEntitylink plugins states vc gc source table link
                    :: (table
                            |> Maybe.map (browseAddresslinkTable vc gc currency)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Label _ table ->
                table
                    |> table_ vc Nothing (LabelAddressTagsTable.config vc)
                    |> List.singleton

            Browser.UserTags table ->
                table
                    |> table_ vc cm (UserAddressTagsTable.config vc)
                    |> List.singleton

            Browser.Plugin ->
                let
                    hasNode node =
                        case node of
                            Node.Address address ->
                                Layer.getFirstAddress address model.layers
                                    |> (/=) Nothing

                            Node.Entity entity ->
                                Layer.getFirstEntity entity model.layers
                                    |> (/=) Nothing
                in
                browsePlugin plugins vc gc hasNode states
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


browseRow : View.Config -> (r -> Html msg) -> Row r Coords msg -> Html msg
browseRow vc map row =
    case row of
        Rule ->
            rule vc

        Image muri ->
            div
                [ Css.propertyBoxRow vc False |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    []
                , span
                    []
                    [ case muri of
                        Just uri ->
                            let
                                uriWithPrefix =
                                    addProtocolPrefx uri
                            in
                            {- Setting a default image see https://stackoverflow.com/questions/980855/inputting-a-default-image-in-case-the-src-attribute-of-an-html-img-is-not-vali -}
                            object [ attribute "data" uriWithPrefix, Css.propertyBoxImage vc |> css ]
                                [ img [ src vc.theme.userDefautImgUrl, Css.propertyBoxImage vc |> css ] []
                                ]

                        Nothing ->
                            img [ src vc.theme.userDefautImgUrl, Css.propertyBoxImage vc |> css ] []
                    ]
                ]

        Note note ->
            div
                [ Css.propertyBoxRow vc False |> css
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

        Footnote note ->
            div
                [ Css.propertyBoxRow vc False |> css
                ]
                [ span
                    [ Css.propertyBoxKey vc |> css
                    ]
                    []
                , span
                    [ (Css.propertyBoxNote vc ++ [ CssStyled.fontSize <| CssStyled.em 0.8 ]) |> css ]
                    [ div [ [ CssStyled.textAlign CssStyled.right ] |> css ] [ text note ]
                    ]
                ]

        Row ( key, value, table ) ->
            div
                [ table |> Maybe.map .active |> Maybe.withDefault False |> Css.propertyBoxRow vc |> css
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

        RowWithMoreActionsButton ( key, value, msg ) ->
            div
                [ Css.propertyBoxRow vc False |> css
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
                        , msg
                            |> Maybe.map
                                (\vmsg ->
                                    div
                                        [ Locale.string vc.locale "more actions" |> title
                                        , on "click" (Util.Graph.decodeCoords Coords |> JD.map vmsg)
                                        , Css.propertyBoxTableLink vc False |> css
                                        ]
                                        [ FontAwesome.IconLayer FontAwesome.caretSquareDown FontAwesome.Solid [] []
                                            |> propertyBoxButton False
                                        ]
                                )
                            |> Maybe.withDefault (div [] [])
                        ]
                    ]
                ]

        OptionalRow optionalRow bool ->
            if bool then
                browseRow vc map optionalRow

            else
                span [] []


tableLink : View.Config -> TableLink -> Html msg
tableLink vc link =
    a
        [ Css.propertyBoxTableLink vc link.active |> css
        , href link.link
        , title link.title
        ]
        [ FontAwesome.IconLayer FontAwesome.ellipsisH FontAwesome.Solid [] []
            |> propertyBoxButton link.active
        ]


propertyBoxButton : Bool -> FontAwesome.IconLayer msg -> Html msg
propertyBoxButton active iconlayer =
    FontAwesome.layers
        [ iconlayer
        , FontAwesome.IconLayer FontAwesome.caretRight
            FontAwesome.Solid
            [ FontAwesome.Pull FontAwesome.Right ]
            [ Html.Attributes.style "opacity" <|
                if active then
                    "1"

                else
                    "0"
            ]
        ]
        []
        |> Html.fromUnstyled


browseValue : View.Config -> Value msg -> Html msg
browseValue vc value =
    case value of
        Stack values ->
            ul [] (List.map (\val -> li [] [ browseValue vc val ]) values)

        Grid width values ->
            let
                gvalues =
                    List.Extra.greedyGroupsOf width values

                viewRow row =
                    li [] [ List.map (browseValue vc) row |> span [] ]
            in
            ul [] (List.map viewRow gvalues)

        String str ->
            div [ css [ CssStyled.minHeight <| CssStyled.em 1 ] ]
                [ text str ]

        HashStr str ->
            copyableLongIdentifier vc [ css [ CssStyled.minHeight <| CssStyled.em 1 ] ] str

        AddressStr str ->
            copyableLongIdentifier vc [ css [ CssStyled.minHeight <| CssStyled.em 1 ] ] str

        Country isocode name ->
            span [ css [ CssStyled.minHeight <| CssStyled.em 1, CssStyled.paddingRight <| CssStyled.em 1 ], title name ]
                [ span [ css [ CssStyled.fontSize <| CssStyled.em 1.2, CssStyled.marginRight <| CssStyled.em 0.2 ] ] [ getFlagEmoji isocode |> text ]
                , span [ css [ CssStyled.fontFamily <| CssStyled.monospace ] ] [ text isocode ]
                ]

        Uri lbl uri ->
            a [ href (addProtocolPrefx uri), target "_blank", CssView.link vc |> css ]
                [ text lbl ]

        IconLink icon uri ->
            a [ href (addProtocolPrefx uri), target "_blank", CssView.iconLink vc |> css ]
                [ FontAwesome.icon icon |> Html.fromUnstyled ]

        InternalLink lbl uri ->
            a [ href uri, CssView.link vc |> css ]
                [ text lbl ]

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

        Select options msg current ->
            options
                |> List.map
                    (\( key, title ) ->
                        option
                            [ Html.Styled.Attributes.value key
                            , current == key |> selected
                            ]
                            [ Locale.string vc.locale title
                                |> text
                            ]
                    )
                |> select
                    [ CssView.input vc |> css
                    , onInput msg
                    ]

        EntityId gc entity ->
            div
                []
                [ Maybe.Extra.orListLazy
                    [ \() ->
                        Model.Graph.Entity.getBestActor entity
                            |> Maybe.map
                                (\actor -> actorLink vc actor.id actor.label)
                    , \() ->
                        entity.entity.bestAddressTag
                            |> Maybe.map
                                (\tag ->
                                    span
                                        [ tag.category
                                            |> Maybe.map
                                                (vc.theme.graph.categoryToColor
                                                    >> toCssColor
                                                    >> CssStyled.color
                                                    >> List.singleton
                                                )
                                            |> Maybe.withDefault []
                                            |> css
                                        ]
                                        [ text
                                            (if String.isEmpty tag.label && not tag.tagpackIsPublic then
                                                tag.category
                                                    |> Maybe.andThen (View.getConceptName vc)
                                                    |> Maybe.withDefault (Locale.string vc.locale "Tag locked")

                                             else
                                                tag.label
                                            )
                                        ]
                                )
                    ]
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
            case vc.locale.valueDetail of
                Locale.Exact ->
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

                Locale.Magnitude ->
                    div
                        []
                        [ span
                            [ Css.propertyBoxUsageRelative vc |> css
                            ]
                            [ Locale.relativeTime vc.locale now timestamp
                                |> text
                            ]
                        ]

        Duration dur ->
            span
                [ Css.propertyBoxActivityPeriod vc |> css
                ]
                [ 1000 * dur |> Locale.durationToString vc.locale |> text
                ]

        Value coinMap ->
            span
                []
                [ Locale.currency vc.locale coinMap
                    |> text
                ]

        MultiValue gc parentCoin len values ->
            values
                |> List.filter (\( c, v ) -> filterTxValue gc c.network v Nothing)
                |> List.map
                    (\( coinCode, v ) ->
                        let
                            cc =
                                if Data.isAccountLike parentCoin then
                                    coinCode.asset

                                else
                                    case vc.locale.currency of
                                        Currency.Coin ->
                                            parentCoin

                                        Currency.Fiat fiat ->
                                            fiat
                        in
                        tr []
                            [ String.toUpper cc
                                |> text
                                |> List.singleton
                                |> td [ Css.currencyCell vc |> css ]
                            , multiValue vc (asset parentCoin cc) v
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


browseAddress : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Maybe AddressTable -> Loadable String Address -> Html Msg
browseAddress plugins states vc gc now table address =
    (rowsAddress vc now table address |> properties vc)
        ++ [ rule vc ]
        ++ (case address of
                Loading _ _ ->
                    []

                Loaded ad ->
                    Plugin.View.addressProperties plugins states ad.plugins vc gc
           )
        |> propertyBox vc


properties : View.Config -> List (Row (Value msg) Coords msg) -> List (Html msg)
properties vc =
    List.map (browseRow vc (browseValue vc))


rowsAddress : View.Config -> Time.Posix -> Maybe AddressTable -> Loadable String Address -> List (Row (Value Msg) Coords Msg)
rowsAddress vc now table address =
    let
        layer =
            address
                |> Loadable.map (.id >> Id.layer >> Just)
                |> Loadable.withDefault Nothing

        mkTableLink title tableTag =
            address
                |> makeTableLink
                    (.address >> .currency)
                    (.address >> .address)
                    (\currency id ->
                        let
                            active =
                                unwrapTableRouteMatch matchTableRouteToAddressTable table tableTag
                        in
                        { title = Locale.string vc.locale title
                        , link =
                            Route.addressRoute
                                { currency = currency
                                , address = id
                                , table =
                                    if active then
                                        Nothing

                                    else
                                        Just tableTag
                                , layer = layer
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = active
                        }
                    )

        rowsPart1 =
            [ Row
                ( "Tags"
                , address
                    |> Loadable.map
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
                    |> Loadable.map
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
                ( "Sending addresses"
                , address
                    |> Loadable.map (.address >> .inDegree >> Locale.int vc.locale >> String)
                    |> elseLoading
                , mkTableLink "List sending addresses" Route.AddressIncomingNeighborsTable
                )
            , Row
                ( "Receiving addresses"
                , address
                    |> Loadable.map (.address >> .outDegree >> Locale.int vc.locale >> String)
                    |> elseLoading
                , mkTableLink "List receiving addresses" Route.AddressOutgoingNeighborsTable
                )
            ]

        rowsPart2 =
            [ Row
                ( "Last usage"
                , address
                    |> Loadable.map (.address >> .lastTx >> .timestamp >> Usage now)
                    |> elseLoading
                , Nothing
                )
            , Row
                ( "Activity period"
                , address
                    |> Loadable.map
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
                    |> Loadable.map
                        (totalReceivedValues .address
                            >> Value
                        )
                    |> elseLoading
                , mkTableLink "Total received assets" Route.AddressTotalReceivedAllAssetsTable
                )
            , Row
                ( "Final balance"
                , address
                    |> Loadable.map
                        (balanceValues .address
                            >> Value
                        )
                    |> elseLoading
                , mkTableLink "Final balance assets" Route.AddressFinalBalanceAllAssetsTable
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

        betaIndicator =
            [ OptionalRow (Footnote "BETA")
                (case address of
                    Loaded a ->
                        a.address.currency == "trx"

                    _ ->
                        False
                )
            ]

        len =
            multiValueMaxLen vc .address address
    in
    [ RowWithMoreActionsButton
        ( "Address"
        , address
            |> Loadable.map (.address >> .address >> AddressStr)
            |> elseShowAddress
        , case address of
            Loaded addr ->
                Just (UserClickedAddressActions addr.id)

            _ ->
                Nothing
        )
    , OptionalRow
        (Row
            ( "Actor"
            , address
                |> Loadable.map
                    (.address
                        >> .actors
                        >> Maybe.withDefault []
                        >> List.map (\x -> InternalLink x.label (Route.actorRoute x.id Nothing |> Route.graphRoute |> toUrl))
                        >> Stack
                    )
                |> elseLoading
            , Nothing
            )
        )
        (case address of
            Loaded a ->
                List.length (a.address.actors |> Maybe.withDefault []) > 0

            _ ->
                False
        )
    , Row
        ( "Currency"
        , address
            |> Loadable.map (.address >> .currency >> String.toUpper >> String)
            |> elseShowCurrency
        , Nothing
        )
    ]
        ++ (if loadableAddress address |> .currency |> Data.isAccountLike then
                [ Row
                    ( "Smart contract"
                    , address
                        |> Loadable.map
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
                    |> Loadable.map (.address >> .firstTx >> .timestamp >> Usage now)
                    |> elseLoading
                , Nothing
                )
           ]
        ++ dataPart2
        ++ statusNote



-- ++ betaIndicator


unwrapTableRouteMatch : (table -> route -> Bool) -> Maybe table -> route -> Bool
unwrapTableRouteMatch match =
    Maybe.map match
        >> Maybe.withDefault (always False)


matchTableRouteToAddressTable : AddressTable -> Route.AddressTable -> Bool
matchTableRouteToAddressTable table route =
    case ( table, route ) of
        ( AddressTagsTable _, Route.AddressTagsTable ) ->
            True

        ( AddressTxsUtxoTable _, Route.AddressTxsTable ) ->
            True

        ( AddressTxsAccountTable _, Route.AddressTxsTable ) ->
            True

        ( AddressIncomingNeighborsTable _, Route.AddressIncomingNeighborsTable ) ->
            True

        ( AddressOutgoingNeighborsTable _, Route.AddressOutgoingNeighborsTable ) ->
            True

        ( AddressTotalReceivedAllAssetsTable _, Route.AddressTotalReceivedAllAssetsTable ) ->
            True

        ( AddressFinalBalanceAllAssetsTable _, Route.AddressFinalBalanceAllAssetsTable ) ->
            True

        ( AddressTagsTable _, _ ) ->
            False

        ( AddressTxsUtxoTable _, _ ) ->
            False

        ( AddressTxsAccountTable _, _ ) ->
            False

        ( AddressIncomingNeighborsTable _, _ ) ->
            False

        ( AddressOutgoingNeighborsTable _, _ ) ->
            False

        ( AddressTotalReceivedAllAssetsTable _, _ ) ->
            False

        ( AddressFinalBalanceAllAssetsTable _, _ ) ->
            False


matchTableRouteToEntityTable : EntityTable -> Route.EntityTable -> Bool
matchTableRouteToEntityTable table route =
    case ( table, route ) of
        ( EntityTagsTable _, Route.EntityTagsTable ) ->
            True

        ( EntityTxsUtxoTable _, Route.EntityTxsTable ) ->
            True

        ( EntityTxsAccountTable _, Route.EntityTxsTable ) ->
            True

        ( EntityIncomingNeighborsTable _, Route.EntityIncomingNeighborsTable ) ->
            True

        ( EntityOutgoingNeighborsTable _, Route.EntityOutgoingNeighborsTable ) ->
            True

        ( EntityTotalReceivedAllAssetsTable _, Route.EntityTotalReceivedAllAssetsTable ) ->
            True

        ( EntityFinalBalanceAllAssetsTable _, Route.EntityFinalBalanceAllAssetsTable ) ->
            True

        ( EntityAddressesTable _, Route.EntityAddressesTable ) ->
            True

        ( EntityTagsTable _, _ ) ->
            False

        ( EntityTxsUtxoTable _, _ ) ->
            False

        ( EntityTxsAccountTable _, _ ) ->
            False

        ( EntityIncomingNeighborsTable _, _ ) ->
            False

        ( EntityOutgoingNeighborsTable _, _ ) ->
            False

        ( EntityTotalReceivedAllAssetsTable _, _ ) ->
            False

        ( EntityFinalBalanceAllAssetsTable _, _ ) ->
            False

        ( EntityAddressesTable _, _ ) ->
            False


matchTableRouteToAddresslinkTable : AddresslinkTable -> Route.AddresslinkTable -> Bool
matchTableRouteToAddresslinkTable table route =
    case ( table, route ) of
        ( AddresslinkTxsUtxoTable _, Route.AddresslinkTxsTable ) ->
            True

        ( AddresslinkTxsAccountTable _, Route.AddresslinkTxsTable ) ->
            True

        ( AddresslinkAllAssetsTable _, Route.AddresslinkAllAssetsTable ) ->
            True

        ( AddresslinkTxsUtxoTable _, _ ) ->
            False

        ( AddresslinkTxsAccountTable _, _ ) ->
            False

        ( AddresslinkAllAssetsTable _, _ ) ->
            False


matchTableRouteToBlockTable : BlockTable -> Route.BlockTable -> Bool
matchTableRouteToBlockTable table route =
    case ( table, route ) of
        ( BlockTxsUtxoTable _, Route.BlockTxsTable ) ->
            True

        ( BlockTxsAccountTable _, Route.BlockTxsTable ) ->
            True


matchTableRouteToActorTable : ActorTable -> Route.ActorTable -> Bool
matchTableRouteToActorTable table route =
    case ( table, route ) of
        ( ActorTagsTable _, Route.ActorTagsTable ) ->
            True

        ( ActorOtherLinksTable _, Route.ActorOtherLinksTable ) ->
            True

        ( ActorTagsTable _, _ ) ->
            False

        ( ActorOtherLinksTable _, _ ) ->
            False


matchTableRouteToTxUtxoTable : TxUtxoTable -> Route.TxTable -> Bool
matchTableRouteToTxUtxoTable table route =
    case ( table, route ) of
        ( TxUtxoInputsTable _, Route.TxInputsTable ) ->
            True

        ( TxUtxoOutputsTable _, Route.TxOutputsTable ) ->
            True

        ( TxUtxoInputsTable _, _ ) ->
            False

        ( TxUtxoOutputsTable _, _ ) ->
            False


matchTableRouteToTxAccountTable : TxAccountTable -> Route.TxTable -> Bool
matchTableRouteToTxAccountTable table route =
    case ( table, route ) of
        ( TokenTxsTable _, Route.TokenTxsTable ) ->
            True

        ( TokenTxsTable _, _ ) ->
            False


makeTableLink : (a -> String) -> (a -> id) -> (String -> id -> TableLink) -> Loadable id a -> Maybe TableLink
makeTableLink getCurrency getId make l =
    case l of
        Loading curr id ->
            make curr id
                |> Just

        Loaded a ->
            make (getCurrency a) (getId a)
                |> Just


elseLoading : Loadable id (Value msg) -> Value msg
elseLoading =
    Loadable.withDefault LoadingValue


elseShowAddress : Loadable String (Value msg) -> Value msg
elseShowAddress l =
    case l of
        Loading _ id ->
            String (truncateLongIdentifier id)

        Loaded v ->
            v


elseShowTxAccount : Loadable ( String, Maybe Int ) (Value msg) -> Value msg
elseShowTxAccount l =
    case l of
        Loading _ ( id, _ ) ->
            String (truncateLongIdentifier id)

        Loaded v ->
            v


elseShowCurrency : Loadable id (Value msg) -> Value msg
elseShowCurrency l =
    case l of
        Loading currency _ ->
            String <| String.toUpper currency

        Loaded v ->
            v


browseEntity : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Maybe EntityTable -> Loadable Int Entity -> Html Msg
browseEntity plugins states vc gc now table entity =
    (rowsEntity vc gc now table entity |> List.map (browseRow vc (browseValue vc)))
        ++ [ rule vc ]
        ++ (case entity of
                Loading _ _ ->
                    []

                Loaded en ->
                    Plugin.View.entityProperties plugins states en.plugins vc gc
           )
        |> propertyBox vc


browseActor : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Maybe ActorTable -> Loadable String Actor -> Html Msg
browseActor plugins states vc gc now table actor =
    (rowsActor vc gc now table actor |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


browseBlock : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Maybe BlockTable -> Loadable Int Api.Data.Block -> Html Msg
browseBlock plugins states vc gc now table block =
    (rowsBlock vc gc now table block |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


browseTxUtxo : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Maybe TxUtxoTable -> Loadable String Api.Data.TxUtxo -> Html Msg
browseTxUtxo plugins states vc gc now table tx =
    (rowsTxUtxo vc gc now table tx |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


browseTxAccount : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable ( String, Maybe Int ) Api.Data.TxAccount -> Maybe TxAccountTable -> String -> Html Msg
browseTxAccount plugins states vc gc now tx table coinCode =
    (rowsTxAccount vc gc now tx table coinCode |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


rowsEntity : View.Config -> Graph.Config -> Time.Posix -> Maybe EntityTable -> Loadable Int Entity -> List (Row (Value Msg) Coords Msg)
rowsEntity vc gc now table ent =
    let
        layer =
            ent
                |> Loadable.map (.id >> Id.layer >> Just)
                |> Loadable.withDefault Nothing

        mkTableLink title tableTag =
            ent
                |> makeTableLink
                    (.entity >> .currency)
                    (.entity >> .entity)
                    (\currency id ->
                        let
                            active =
                                unwrapTableRouteMatch matchTableRouteToEntityTable table tableTag
                        in
                        { title = Locale.string vc.locale title
                        , link =
                            Route.entityRoute
                                { currency = currency
                                , entity = id
                                , table =
                                    if active then
                                        Nothing

                                    else
                                        Just tableTag
                                , layer = layer
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = active
                        }
                    )

        -- betaIndicator =
        --     [ OptionalRow (Footnote "BETA")
        --         (case ent of
        --             Loaded e ->
        --                 e.entity.currency == "trx"
        --             _ ->
        --                 False
        --         )
        --     ]
        -- len =
        --     multiValueMaxLen vc .entity ent
    in
    [ RowWithMoreActionsButton
        ( "Entity"
        , ent |> Loadable.map (EntityId gc) |> elseLoading
        , case ent of
            Loaded entity ->
                Just (UserClickedEntityActions entity.id)

            _ ->
                Nothing
        )
    , Row
        ( "Root Address"
        , ent |> Loadable.map (.entity >> .rootAddress >> AddressStr) |> elseLoading
        , Nothing
        )

    {- , OptionalRow
       (Row
           ( "Actors"
           , ent
               |> Loadable.map
                   (.entity
                       >> .actors
                       >> Maybe.withDefault []
                       >> List.map
                           (\x ->
                               InternalLink x.label
                                   (Route.actorRoute x.id Nothing
                                       |> Route.graphRoute
                                       |> toUrl
                                   )
                           )
                       >> Stack
                   )
               |> elseLoading
           , Nothing
           )
       )
       (case ent of
           Loaded a ->
               List.length (a.entity.actors |> Maybe.withDefault []) > 0

           _ ->
               False
       )
    -}
    , Row
        ( "Currency"
        , ent |> Loadable.map (.entity >> .currency >> String.toUpper >> String) |> elseShowCurrency
        , Nothing
        )
    , Row
        ( "Addresses"
        , ent
            |> Loadable.map
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
            |> Loadable.map
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
            |> Loadable.map
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
        ( "Sending entities"
        , ent
            |> Loadable.map (\entity -> Locale.int vc.locale entity.entity.inDegree |> String)
            |> elseLoading
        , mkTableLink "List sending entities" Route.EntityIncomingNeighborsTable
        )
    , Row
        ( "Receiving entities"
        , ent
            |> Loadable.map
                (\entity ->
                    Locale.int vc.locale entity.entity.outDegree
                        |> String
                )
            |> elseLoading
        , mkTableLink "List receiving entities" Route.EntityOutgoingNeighborsTable
        )
    , Rule
    , Row
        ( "First usage"
        , ent |> Loadable.map (\entity -> Usage now entity.entity.firstTx.timestamp) |> elseLoading
        , Nothing
        )
    , Row
        ( "Last usage"
        , ent |> Loadable.map (\entity -> Usage now entity.entity.lastTx.timestamp) |> elseLoading
        , Nothing
        )
    , Row
        ( "Activity period"
        , ent
            |> Loadable.map (\entity -> entity.entity.firstTx.timestamp - entity.entity.lastTx.timestamp |> Duration)
            |> elseLoading
        , Nothing
        )
    , Rule
    , Row
        ( "Total received"
        , ent
            |> Loadable.map
                (totalReceivedValues .entity
                    >> Value
                )
            |> elseLoading
        , ent
            |> Loadable.map
                (totalReceivedValues .entity
                    >> List.drop 1
                    >> List.head
                    >> Maybe.andThen
                        (\_ ->
                            mkTableLink "Total received assets" Route.EntityTotalReceivedAllAssetsTable
                        )
                )
            |> Loadable.withDefault Nothing
        )
    , Row
        ( "Final balance"
        , ent
            |> Loadable.map
                (balanceValues .entity
                    >> Value
                )
            |> elseLoading
        , ent
            |> Loadable.map
                (totalReceivedValues .entity
                    >> List.drop 1
                    >> List.head
                    >> Maybe.andThen
                        (\_ ->
                            mkTableLink "Final balance assets" Route.EntityFinalBalanceAllAssetsTable
                        )
                )
            |> Loadable.withDefault Nothing
        )
    ]



-- ++ betaIndicator


rowsActor : View.Config -> Graph.Config -> Time.Posix -> Maybe ActorTable -> Loadable String Actor -> List (Row (Value Msg) Coords Msg)
rowsActor vc gc now table actor =
    let
        mkTableLink title tableTag =
            actor
                |> makeTableLink
                    (\_ -> "")
                    .id
                    (\_ id ->
                        let
                            active =
                                unwrapTableRouteMatch matchTableRouteToActorTable table tableTag
                        in
                        { title = Locale.string vc.locale title
                        , link =
                            Route.actorRoute id
                                (if active then
                                    Nothing

                                 else
                                    Just tableTag
                                )
                                |> Route.graphRoute
                                |> toUrl
                        , active = active
                        }
                    )
    in
    [ Image
        (case actor of
            Loaded a ->
                getImageUri a

            _ ->
                Nothing
        )
    , Rule
    , Row ( "Actor", actor |> Loadable.map (.label >> String) |> elseLoading, Nothing )
    , Rule
    , Row ( "Url", actor |> Loadable.map (.uri >> (\x -> Uri x x)) |> elseLoading, Nothing )
    , Rule
    , Row
        ( "Categories"
        , actor
            |> Loadable.map
                (.categories
                    >> List.map .label
                    >> List.map String
                    >> Stack
                )
            |> elseLoading
        , Nothing
        )
    , Rule
    , OptionalRow
        (Row
            ( "Jurisdictions"
            , actor
                |> Loadable.map
                    (.jurisdictions
                        >> List.map (\x -> Country x.id x.label)
                        >> Grid 3
                    )
                |> elseLoading
            , Nothing
            )
        )
        (case actor of
            Loaded a ->
                List.length a.jurisdictions > 0

            _ ->
                False
        )
    , Rule
    , OptionalRow
        (Row
            ( "Social"
            , actor
                |> Loadable.map
                    (getUrisWithoutMain
                        >> getFontAwesomeIconForUris
                        >> List.filter (\( _, icon ) -> icon /= Nothing)
                        >> List.map (\( uri, icon ) -> IconLink (icon |> Maybe.withDefault FontAwesome.question) uri)
                        >> Grid 7
                    )
                |> elseLoading
            , Nothing
            )
        )
        (case actor of
            Loaded a ->
                List.length
                    (a
                        |> getUrisWithoutMain
                        |> getFontAwesomeIconForUris
                        |> List.filter (\( _, icon ) -> icon /= Nothing)
                    )
                    > 0

            _ ->
                False
        )
    , Rule
    , Row
        ( "Other Links"
        , actor
            |> Loadable.map
                ((\_ -> "") >> String)
            |> elseLoading
        , mkTableLink "More links" Route.ActorOtherLinksTable
        )
    , Rule
    , Row
        ( "Tags"
        , actor
            |> Loadable.map
                (.nrTags
                    >> Maybe.map String.fromInt
                    >> Maybe.withDefault "-"
                    >> String
                )
            |> elseLoading
        , mkTableLink "Show Actor Tags" Route.ActorTagsTable
        )
    , OptionalRow (Footnote "Logo provided by CoinGecko")
        (case actor of
            Loaded a ->
                getImageUri a |> Maybe.map (String.contains "coingecko") |> Maybe.withDefault False

            _ ->
                False
        )
    ]


rowsBlock : View.Config -> Graph.Config -> Time.Posix -> Maybe BlockTable -> Loadable Int Api.Data.Block -> List (Row (Value Msg) Coords Msg)
rowsBlock vc gc now table block =
    let
        mkTableLink title tableTag =
            block
                |> makeTableLink
                    .currency
                    .height
                    (\currency id ->
                        let
                            active =
                                unwrapTableRouteMatch matchTableRouteToBlockTable table tableTag
                        in
                        { title = Locale.string vc.locale title
                        , link =
                            Route.blockRoute
                                { currency = currency
                                , block = id
                                , table =
                                    if active then
                                        Nothing

                                    else
                                        Just tableTag
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = active
                        }
                    )
    in
    [ Row ( "Height", block |> Loadable.map (.height >> Locale.int vc.locale >> String) |> elseLoading, Nothing )
    , Row
        ( "Currency"
        , block
            |> Loadable.map (.currency >> String.toUpper >> String)
            |> elseShowCurrency
        , Nothing
        )
    , Row
        ( "Transactions"
        , block
            |> Loadable.map (.noTxs >> Locale.int vc.locale >> String)
            |> elseLoading
        , mkTableLink "List block transactions" Route.BlockTxsTable
        )
    , Row
        ( "Block hash"
        , block
            |> Loadable.map (.blockHash >> HashStr)
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Created"
        , block |> Loadable.map (.timestamp >> Locale.timestamp vc.locale >> String) |> elseLoading
        , Nothing
        )
    ]


browseAddressTable : View.Config -> Graph.Config -> (Id.AddressId -> Bool -> A.Address -> Bool) -> Loadable String Address -> AddressTable -> Html Msg
browseAddressTable vc gc neighborLayerHasAddress address table =
    let
        ( coinCode, addressId ) =
            case address of
                Loaded a ->
                    ( a.address.currency, a.id |> Just )

                Loading curr _ ->
                    ( curr, Nothing )

        tt =
            table_ vc cm
    in
    case table of
        AddressTxsUtxoTable t ->
            tt (AddressTxsUtxoTable.config vc coinCode) t

        AddressTxsAccountTable t ->
            tt (TxsAccountTable.config vc coinCode) t

        AddressTagsTable t ->
            table_ vc Nothing (AddressTagsTable.config vc Nothing Nothing (\_ _ -> False)) t

        AddressIncomingNeighborsTable t ->
            tt (AddressNeighborsTable.config vc False coinCode addressId neighborLayerHasAddress) t

        AddressOutgoingNeighborsTable t ->
            tt (AddressNeighborsTable.config vc True coinCode addressId neighborLayerHasAddress) t

        AddressTotalReceivedAllAssetsTable t ->
            tt (AllAssetsTable.config vc) t

        AddressFinalBalanceAllAssetsTable t ->
            tt (AllAssetsTable.config vc) t


table_ : View.Config -> Maybe Msg -> Table.Config data Msg -> Table data -> Html Msg
table_ vc csvMsg =
    Table.table styles
        vc
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


browseEntityTable : View.Config -> Graph.Config -> (Id.EntityId -> A.Address -> Bool) -> (Id.EntityId -> Bool -> E.Entity -> Bool) -> Loadable Int Entity -> EntityTable -> Html Msg
browseEntityTable vc gc entityHasAddress neighborLayerHasEntity entity table =
    let
        ( coinCode, entityId, bestAddressTag ) =
            case entity of
                Loaded e ->
                    ( e.entity.currency, e.id |> Just, e.entity.bestAddressTag )

                Loading curr _ ->
                    ( curr, Nothing, Nothing )

        tt =
            table_ vc cm
    in
    case table of
        EntityAddressesTable t ->
            tt (EntityAddressesTable.config vc coinCode entityId entityHasAddress) t

        EntityTxsUtxoTable t ->
            tt (AddressTxsUtxoTable.config vc coinCode) t

        EntityTxsAccountTable t ->
            tt (TxsAccountTable.config vc coinCode) t

        EntityTagsTable t ->
            table_ vc Nothing (AddressTagsTable.config vc bestAddressTag entityId entityHasAddress) t

        EntityIncomingNeighborsTable t ->
            tt (EntityNeighborsTable.config vc False coinCode entityId neighborLayerHasEntity) t

        EntityOutgoingNeighborsTable t ->
            tt (EntityNeighborsTable.config vc True coinCode entityId neighborLayerHasEntity) t

        EntityTotalReceivedAllAssetsTable t ->
            tt (AllAssetsTable.config vc) t

        EntityFinalBalanceAllAssetsTable t ->
            tt (AllAssetsTable.config vc) t


browseActorTable : View.Config -> Graph.Config -> Loadable String Actor -> ActorTable -> Html Msg
browseActorTable vc gc actor table =
    case table of
        ActorTagsTable t ->
            table_ vc Nothing (LabelAddressTagsTable.config vc) t

        ActorOtherLinksTable t ->
            table_ vc Nothing (LinksTable.config vc) t


browseBlockTable : View.Config -> Graph.Config -> Loadable Int Api.Data.Block -> BlockTable -> Html Msg
browseBlockTable vc gc block table =
    let
        ( coinCode, _ ) =
            case block of
                Loaded e ->
                    ( e.currency, e.height |> Just )

                Loading curr _ ->
                    ( curr, Nothing )
    in
    case table of
        BlockTxsUtxoTable t ->
            table_ vc cm (TxsUtxoTable.config vc coinCode) t

        BlockTxsAccountTable t ->
            table_ vc cm (TxsAccountTable.blockConfig vc coinCode) t


browseTxUtxoTable : View.Config -> Graph.Config -> Loadable String Api.Data.TxUtxo -> TxUtxoTable -> Html Msg
browseTxUtxoTable vc gc tx table =
    let
        ( coinCode, _ ) =
            case tx of
                Loaded e ->
                    ( e.currency, e.txHash |> Just )

                Loading curr _ ->
                    ( curr, Nothing )
    in
    case table of
        TxUtxoInputsTable t ->
            table_ vc cm (TxUtxoTable.config vc False coinCode) t

        TxUtxoOutputsTable t ->
            table_ vc cm (TxUtxoTable.config vc True coinCode) t


browseTxAccountTable : View.Config -> Graph.Config -> Loadable ( String, Maybe Int ) Api.Data.TxAccount -> TxAccountTable -> Html Msg
browseTxAccountTable vc gc tx (TokenTxsTable table) =
    let
        ( coinCode, _ ) =
            case tx of
                Loaded e ->
                    ( e.currency, e.txHash |> Just )

                Loading curr _ ->
                    ( curr, Nothing )
    in
    table_ vc cm (TxsAccountTable.config vc coinCode) table


browsePlugin : Plugins -> View.Config -> Graph.Config -> (Node.Node A.Address E.Entity -> Bool) -> ModelState -> List (Html Msg)
browsePlugin plugins vc gc hasNode states =
    Plugin.View.browser plugins vc gc hasNode states


rowsTxUtxo : View.Config -> Graph.Config -> Time.Posix -> Maybe TxUtxoTable -> Loadable String Api.Data.TxUtxo -> List (Row (Value Msg) Coords Msg)
rowsTxUtxo vc gc now table tx =
    let
        mkTableLink title tableTag =
            tx
                |> makeTableLink
                    .currency
                    .txHash
                    (\currency id ->
                        let
                            active =
                                unwrapTableRouteMatch matchTableRouteToTxUtxoTable table tableTag
                        in
                        { title = Locale.string vc.locale title
                        , link =
                            Route.txRoute
                                { currency = currency
                                , txHash = id
                                , table =
                                    if active then
                                        Nothing

                                    else
                                        Just tableTag
                                , tokenTxId = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = active
                        }
                    )
    in
    [ RowWithMoreActionsButton
        ( "Transaction"
        , tx
            |> Loadable.map (.txHash >> HashStr)
            |> elseShowAddress
        , case tx of
            Loaded txi ->
                Just (UserClickedTransactionActions txi.txHash txi.currency)

            _ ->
                Nothing
        )
    , Row
        ( "Included in block"
        , tx |> Loadable.map (.height >> Locale.intWithoutValueDetailFormatting vc.locale >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "Created"
        , tx |> Loadable.map (.timestamp >> Locale.timestamp vc.locale >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "No. inputs"
        , tx
            |> Loadable.map
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
            |> Loadable.map
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
            |> Loadable.map
                (\t -> Value [ ( assetFromBase t.currency, t.totalInput ) ])
            |> elseLoading
        , Nothing
        )
    , Row
        ( "total output"
        , tx
            |> Loadable.map
                (\t -> Value [ ( assetFromBase t.currency, t.totalOutput ) ])
            |> elseLoading
        , Nothing
        )
    ]


rowsTxAccount : View.Config -> Graph.Config -> Time.Posix -> Loadable ( String, Maybe Int ) Api.Data.TxAccount -> Maybe TxAccountTable -> String -> List (Row (Value Msg) Coords Msg)
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
                [ longIdentifier vc (getAddress tx_)
                ]
                |> Html

        mkTableLink title tableTag =
            tx
                |> makeTableLink
                    (\_ -> coinCode)
                    (\d -> ( d.txHash, d.tokenTxId ))
                    (\currency id ->
                        let
                            active =
                                unwrapTableRouteMatch matchTableRouteToTxAccountTable table tableTag
                        in
                        { title = Locale.string vc.locale title
                        , link =
                            Route.txRoute
                                { currency = currency
                                , txHash = first id
                                , table =
                                    if active then
                                        Nothing

                                    else
                                        Just tableTag
                                , tokenTxId = Nothing
                                }
                                |> Route.graphRoute
                                |> toUrl
                        , active = active
                        }
                    )
    in
    [ RowWithMoreActionsButton
        ( "Transaction"
        , tx
            |> Loadable.map (.txHash >> HashStr)
            |> elseShowTxAccount
        , case tx of
            Loaded txi ->
                Just (UserClickedTransactionActions txi.txHash txi.currency)

            _ ->
                Nothing
        )
    , Row
        ( "Value"
        , tx
            |> Loadable.map
                (\t -> Value [ ( asset coinCode t.currency, t.value ) ])
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Included in block"
        , tx |> Loadable.map (.height >> Locale.intWithoutValueDetailFormatting vc.locale >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "Created"
        , tx |> Loadable.map (.timestamp >> Locale.timestamp vc.locale >> String) |> elseLoading
        , Nothing
        )
    , Row
        ( "Sending address"
        , tx
            |> Loadable.map (txLink .fromAddress)
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Receiving address"
        , tx
            |> Loadable.map (txLink .toAddress)
            |> elseLoading
        , Nothing
        )
    ]
        ++ (if Data.isAccountLike (loadableCurrency tx) then
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


browseAddresslink : Plugins -> ModelState -> View.Config -> Graph.Config -> Address -> Maybe AddresslinkTable -> Link Address -> Html Msg
browseAddresslink plugins states vc gc source table link =
    (rowsAddresslink vc gc source table link |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


rowsAddresslink : View.Config -> Graph.Config -> Address -> Maybe AddresslinkTable -> Link Address -> List (Row (Value Msg) Coords Msg)
rowsAddresslink vc gc source table link =
    let
        currency =
            Id.currency source.id

        linkData =
            case link.link of
                Link.LinkData ld ->
                    Just ld

                Link.PlaceholderLinkData ->
                    Nothing

        addresslinkRouteBase =
            { currency = currency
            , src = Id.addressId source.id
            , srcLayer = Id.layer source.id
            , dst = Id.addressId link.node.id
            , dstLayer = Id.layer link.node.id
            , table = Nothing
            }
    in
    [ Row
        ( "Source"
        , source.id
            |> Id.addressId
            |> AddressStr
        , Nothing
        )
    , Row
        ( "Target"
        , link.node.id
            |> Id.addressId
            |> AddressStr
        , Nothing
        )
    , Row
        ( "Transactions"
        , linkData
            |> Maybe.map
                (.noTxs >> Locale.int vc.locale)
            |> Maybe.withDefault ""
            |> String
        , let
            active =
                unwrapTableRouteMatch matchTableRouteToAddresslinkTable table Route.AddresslinkTxsTable
          in
          Just
            { title = Locale.string vc.locale "Transactions"
            , link =
                addresslinkRouteBase
                    |> s_table
                        (if active then
                            Nothing

                         else
                            Just Route.AddresslinkTxsTable
                        )
                    |> Route.addresslinkRoute
                    |> Route.graphRoute
                    |> toUrl
            , active = active
            }
        )
    , let
        active =
            unwrapTableRouteMatch matchTableRouteToAddresslinkTable table Route.AddresslinkAllAssetsTable
      in
      linkValueRow vc
        gc
        currency
        linkData
        { title = Locale.string vc.locale "All assets"
        , link =
            addresslinkRouteBase
                |> s_table
                    (if active then
                        Nothing

                     else
                        Just Route.AddresslinkAllAssetsTable
                    )
                |> Route.addresslinkRoute
                |> Route.graphRoute
                |> toUrl
        , active = active
        }
    ]


browseEntitylink : Plugins -> ModelState -> View.Config -> Graph.Config -> Entity -> Maybe AddresslinkTable -> Link Entity -> Html Msg
browseEntitylink plugins states vc gc source table link =
    (rowsEntitylink vc gc source table link |> List.map (browseRow vc (browseValue vc)))
        |> propertyBox vc


rowsEntitylink : View.Config -> Graph.Config -> Entity -> Maybe AddresslinkTable -> Link Entity -> List (Row (Value Msg) Coords Msg)
rowsEntitylink vc gc source table link =
    let
        currency =
            Id.currency source.id

        linkData =
            case link.link of
                Link.LinkData ld ->
                    Just ld

                Link.PlaceholderLinkData ->
                    Nothing

        entitylinkRouteBase =
            { currency = currency
            , src = Id.entityId source.id
            , srcLayer = Id.layer source.id
            , dst = Id.entityId link.node.id
            , dstLayer = Id.layer link.node.id
            , table = Nothing
            }
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
                (.noTxs >> Locale.int vc.locale)
            |> Maybe.withDefault ""
            |> String
        , let
            active =
                unwrapTableRouteMatch matchTableRouteToAddresslinkTable table Route.AddresslinkTxsTable
          in
          Just
            { title = Locale.string vc.locale "Transactions"
            , link =
                entitylinkRouteBase
                    |> s_table
                        (if active then
                            Nothing

                         else
                            Just Route.AddresslinkTxsTable
                        )
                    |> Route.entitylinkRoute
                    |> Route.graphRoute
                    |> toUrl
            , active = active
            }
        )
    , let
        active =
            unwrapTableRouteMatch matchTableRouteToAddresslinkTable table Route.AddresslinkAllAssetsTable
      in
      linkValueRow vc
        gc
        currency
        linkData
        { title = Locale.string vc.locale "All assets"
        , link =
            entitylinkRouteBase
                |> s_table
                    (if active then
                        Nothing

                     else
                        Just Route.AddresslinkAllAssetsTable
                    )
                |> Route.entitylinkRoute
                |> Route.graphRoute
                |> toUrl
        , active = active
        }
    ]


linkValueRow : View.Config -> Graph.Config -> String -> Maybe Link.LinkActualData -> TableLink -> Row (Value Msg) Coords Msg
linkValueRow vc gc parentCurrency linkData tableLink_ =
    let
        values =
            linkData
                |> Maybe.map
                    (\ld ->
                        Label.normalizeValues gc parentCurrency ld.value ld.tokenValues
                    )
    in
    Row
        ( if not (Data.isAccountLike parentCurrency) then
            "Estimated value"

          else
            "Value"
        , values
            |> Maybe.map Value
            |> Maybe.withDefault (String "")
        , values
            |> Maybe.map (List.length >> (<) 1)
            |> Maybe.andThen
                (\moreThanOne ->
                    if moreThanOne then
                        Just tableLink_

                    else
                        Nothing
                )
        )


browseAddresslinkTable : View.Config -> Graph.Config -> String -> AddresslinkTable -> Html Msg
browseAddresslinkTable vc gc coinCode table =
    case table of
        AddresslinkTxsUtxoTable t ->
            table_ vc cm (AddresslinkTxsUtxoTable.config vc coinCode) t

        AddresslinkTxsAccountTable t ->
            table_ vc cm (TxsAccountTable.config vc coinCode) t

        AddresslinkAllAssetsTable t ->
            table_ vc cm (AllAssetsTable.config vc) t


multiValue : View.Config -> Currency.AssetIdentifier -> Api.Data.Values -> String
multiValue vc asset v =
    if Data.isAccountLike asset.network && vc.locale.currency /= Currency.Coin then
        Locale.currency vc.locale [ ( asset, v ) ]

    else
        Locale.currencyWithoutCode vc.locale [ ( asset, v ) ]


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
                |> List.map (\( asset, v ) -> multiValue vc asset v |> String.length)
                |> List.maximum
                |> Maybe.withDefault 0
                |> (+) 2


totalReceivedValues : (thing -> AddressOrEntity a) -> thing -> List ( Currency.AssetIdentifier, Api.Data.Values )
totalReceivedValues accessor a =
    ( (accessor a).currency, (accessor a).totalReceived )
        :: ((accessor a).totalTokensReceived
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []
           )
        |> tokensToValue (accessor a).currency


balanceValues : (thing -> AddressOrEntity a) -> thing -> List ( Currency.AssetIdentifier, Api.Data.Values )
balanceValues accessor a =
    ( (accessor a).currency, (accessor a).balance )
        :: ((accessor a).tokenBalances
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []
           )
        |> tokensToValue (accessor a).currency


tableSeparator : View.Config -> Html msg
tableSeparator vc =
    div
        [ Css.tableSeparator vc |> css
        ]
        []
