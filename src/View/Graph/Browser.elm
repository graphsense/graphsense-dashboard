module View.Graph.Browser exposing (browseRow, browseValue, browser, frame, properties, propertyBox)

import Api.Data exposing (Entity)
import Basics.Extra exposing (uncurry)
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
import List.Extra
import Maybe.Extra
import Model.Address as A
import Model.Currency as Currency
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
import Model.Locale as Locale
import Model.Node as Node
import Msg.Graph exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View exposing (Plugins)
import Route exposing (toUrl)
import Route.Graph as Route
import Table
import Time
import Tuple exposing (..)
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
                browseAddress plugins states vc gc model.now loadable
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
                                    browseEntityTable vc gc entityHasAddress neighborLayerHasEntity loadable t
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Actor loadable table ->
                browseActor plugins states vc gc model.now loadable
                    :: (table
                            |> Maybe.map
                                (\t ->
                                    browseActorTable vc gc loadable t
                                )
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.Block loadable table ->
                browseBlock plugins states vc gc model.now loadable
                    :: (table
                            |> Maybe.map (browseBlockTable vc gc loadable)
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )

            Browser.TxUtxo loadable table ->
                browseTxUtxo plugins states vc gc model.now loadable
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
                browseAddresslink plugins states vc gc source link
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
                browseEntitylink plugins states vc gc source link
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
                    |> table_ vc cm (UserAddressTagsTable.config vc gc)
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
                browsePlugin plugins vc hasNode states
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


browseRow : View.Config -> Graph.Config -> (r -> Html msg) -> Row r Coords msg -> Html msg
browseRow vc gc map row =
    case row of
        Rule ->
            rule vc

        Image muri ->
            div
                [ Css.propertyBoxRow vc |> css
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

        Footnote note ->
            div
                [ Css.propertyBoxRow vc |> css
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

        RowWithMoreActionsButton ( key, value, msg ) ->
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
                        , msg
                            |> Maybe.map
                                (\vmsg ->
                                    div
                                        [ Locale.string vc.locale "more actions" |> title
                                        , on "click" (Util.Graph.decodeCoords Coords |> JD.map vmsg)
                                        , Css.propertyBoxTableLink vc True |> css
                                        , CssView.link vc |> css
                                        ]
                                        [ FontAwesome.icon FontAwesome.caretSquareDown |> Html.fromUnstyled ]
                                )
                            |> Maybe.withDefault (div [] [])
                        ]
                    ]
                ]

        OptionalRow optionalRow bool ->
            if bool then
                browseRow vc gc map optionalRow

            else
                span [] []


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


browseValue : View.Config -> Graph.Config -> Value msg -> Html msg
browseValue vc gc value =
    case value of
        Stack values ->
            ul [] (List.map (\val -> li [] [ browseValue vc gc val ]) values)

        Grid width values ->
            let
                gvalues =
                    List.Extra.greedyGroupsOf width values

                viewRow row =
                    li [] [ List.map (browseValue vc gc) row |> span [] ]
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

        EntityId entity ->
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

        MultiValue parentCoin len values ->
            values
                |> List.filter (\( c, v ) -> filterTxValue gc c v Nothing)
                |> List.map
                    (\( coinCode, v ) ->
                        let
                            cc =
                                if parentCoin == "eth" then
                                    coinCode

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


browseAddress : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable String Address -> Html Msg
browseAddress plugins states vc gc now address =
    (rowsAddress vc now address |> properties vc gc)
        ++ [ rule vc ]
        ++ (case address of
                Loading _ _ ->
                    []

                Loaded ad ->
                    Plugin.View.addressProperties plugins states ad.plugins vc
           )
        |> propertyBox vc


properties : View.Config -> Graph.Config -> List (Row (Value msg) Coords msg) -> List (Html msg)
properties vc gc =
    List.map (browseRow vc gc (browseValue vc gc))


rowsAddress : View.Config -> Time.Posix -> Loadable String Address -> List (Row (Value Msg) Coords Msg)
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
                ( "Sending addresses"
                , address
                    |> ifLoaded (.address >> .inDegree >> Locale.int vc.locale >> String)
                    |> elseLoading
                , mkTableLink "List sending addresses" Route.AddressIncomingNeighborsTable
                )
            , Row
                ( "Receiving addresses"
                , address
                    |> ifLoaded (.address >> .outDegree >> Locale.int vc.locale >> String)
                    |> elseLoading
                , mkTableLink "List receiving addresses" Route.AddressOutgoingNeighborsTable
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
                            >> Value
                        )
                    |> elseLoading
                , Nothing
                )
            , Row
                ( "Final balance"
                , address
                    |> ifLoaded
                        (balanceValues .address
                            >> Value
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
    [ RowWithMoreActionsButton
        ( "Address"
        , address
            |> ifLoaded (.address >> .address >> AddressStr)
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
                |> ifLoaded
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


browseEntity : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable Int Entity -> Html Msg
browseEntity plugins states vc gc now entity =
    (rowsEntity vc gc now entity |> List.map (browseRow vc gc (browseValue vc gc)))
        ++ [ rule vc ]
        ++ (case entity of
                Loading _ _ ->
                    []

                Loaded en ->
                    Plugin.View.entityProperties plugins states en.plugins vc
           )
        |> propertyBox vc


browseActor : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable String Actor -> Html Msg
browseActor plugins states vc gc now actor =
    (rowsActor vc gc now actor |> List.map (browseRow vc gc (browseValue vc gc)))
        |> propertyBox vc


browseBlock : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable Int Api.Data.Block -> Html Msg
browseBlock plugins states vc gc now block =
    (rowsBlock vc gc now block |> List.map (browseRow vc gc (browseValue vc gc)))
        |> propertyBox vc


browseTxUtxo : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable String Api.Data.TxUtxo -> Html Msg
browseTxUtxo plugins states vc gc now tx =
    (rowsTxUtxo vc gc now tx |> List.map (browseRow vc gc (browseValue vc gc)))
        |> propertyBox vc


browseTxAccount : Plugins -> ModelState -> View.Config -> Graph.Config -> Time.Posix -> Loadable ( String, Maybe Int ) Api.Data.TxAccount -> Maybe TxAccountTable -> String -> Html Msg
browseTxAccount plugins states vc gc now tx table coinCode =
    (rowsTxAccount vc gc now tx table coinCode |> List.map (browseRow vc gc (browseValue vc gc)))
        |> propertyBox vc


rowsEntity : View.Config -> Graph.Config -> Time.Posix -> Loadable Int Entity -> List (Row (Value Msg) Coords Msg)
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
    [ RowWithMoreActionsButton
        ( "Entity"
        , ent |> ifLoaded EntityId |> elseLoading
        , case ent of
            Loaded entity ->
                Just (UserClickedEntityActions entity.id)

            _ ->
                Nothing
        )
    , Row
        ( "Root Address"
        , ent |> ifLoaded (.entity >> .rootAddress >> AddressStr) |> elseLoading
        , Nothing
        )

    {- , OptionalRow
       (Row
           ( "Actors"
           , ent
               |> ifLoaded
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
        ( "Sending entities"
        , ent
            |> ifLoaded (\entity -> Locale.int vc.locale entity.entity.inDegree |> String)
            |> elseLoading
        , mkTableLink "List sending entities" Route.EntityIncomingNeighborsTable
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
                    >> Value
                )
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Final balance"
        , ent
            |> ifLoaded
                (balanceValues .entity
                    >> Value
                )
            |> elseLoading
        , Nothing
        )
    ]


rowsActor : View.Config -> Graph.Config -> Time.Posix -> Loadable String Actor -> List (Row (Value Msg) Coords Msg)
rowsActor vc gc now actor =
    let
        mkTableLink title tableTag =
            actor
                |> makeTableLink
                    (\_ -> "")
                    .id
                    (\_ id ->
                        { title = Locale.string vc.locale title
                        , link =
                            Route.actorRoute id (Just tableTag)
                                |> Route.graphRoute
                                |> toUrl
                        , active = False
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
    , Row ( "Actor", actor |> ifLoaded (.label >> String) |> elseLoading, Nothing )
    , Rule
    , Row ( "Url", actor |> ifLoaded (.uri >> (\x -> Uri x x)) |> elseLoading, Nothing )
    , Rule
    , Row
        ( "Categories"
        , actor
            |> ifLoaded
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
                |> ifLoaded
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
                |> ifLoaded
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
            |> ifLoaded
                ((\_ -> "") >> String)
            |> elseLoading
        , mkTableLink "More links" Route.ActorOtherLinksTable
        )
    , Rule
    , Row
        ( "Tags"
        , actor
            |> ifLoaded
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


rowsBlock : View.Config -> Graph.Config -> Time.Posix -> Loadable Int Api.Data.Block -> List (Row (Value Msg) Coords Msg)
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
            |> ifLoaded (.blockHash >> HashStr)
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Created"
        , block |> ifLoaded (.timestamp >> Locale.timestamp vc.locale >> String) |> elseLoading
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
            table_ vc Nothing (AddressTagsTable.config vc gc Nothing Nothing (\_ _ -> False)) t

        AddressIncomingNeighborsTable t ->
            tt (AddressNeighborsTable.config vc False coinCode addressId neighborLayerHasAddress) t

        AddressOutgoingNeighborsTable t ->
            tt (AddressNeighborsTable.config vc True coinCode addressId neighborLayerHasAddress) t


table_ : View.Config -> Maybe Msg -> Table.Config data Msg -> Table data -> Html Msg
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
            table_ vc Nothing (AddressTagsTable.config vc gc bestAddressTag entityId entityHasAddress) t

        EntityIncomingNeighborsTable t ->
            tt (EntityNeighborsTable.config vc False coinCode entityId neighborLayerHasEntity) t

        EntityOutgoingNeighborsTable t ->
            tt (EntityNeighborsTable.config vc True coinCode entityId neighborLayerHasEntity) t


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
            table_ vc cm (TxsAccountTable.config vc coinCode) t


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


browsePlugin : Plugins -> View.Config -> (Node.Node A.Address E.Entity -> Bool) -> ModelState -> List (Html Msg)
browsePlugin plugins vc hasNode states =
    Plugin.View.browser plugins vc hasNode states


rowsTxUtxo : View.Config -> Graph.Config -> Time.Posix -> Loadable String Api.Data.TxUtxo -> List (Row (Value Msg) Coords Msg)
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
    [ RowWithMoreActionsButton
        ( "Transaction"
        , tx
            |> ifLoaded (.txHash >> HashStr)
            |> elseShowAddress
        , case tx of
            Loaded txi ->
                Just (UserClickedTransactionActions txi.txHash txi.currency)

            _ ->
                Nothing
        )
    , Row
        ( "Included in block"
        , tx |> ifLoaded (.height >> Locale.intWithoutValueDetailFormatting vc.locale >> String) |> elseLoading
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
                (\t -> Value [ ( t.currency, t.totalInput ) ])
            |> elseLoading
        , Nothing
        )
    , Row
        ( "total output"
        , tx
            |> ifLoaded
                (\t -> Value [ ( t.currency, t.totalOutput ) ])
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
    [ RowWithMoreActionsButton
        ( "Transaction"
        , tx
            |> ifLoaded (.txHash >> HashStr)
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
            |> ifLoaded
                (\t -> Value [ ( t.currency, t.value ) ])
            |> elseLoading
        , Nothing
        )
    , Row
        ( "Included in block"
        , tx |> ifLoaded (.height >> Locale.intWithoutValueDetailFormatting vc.locale >> String) |> elseLoading
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


browseAddresslink : Plugins -> ModelState -> View.Config -> Graph.Config -> Address -> Link Address -> Html Msg
browseAddresslink plugins states vc gc source link =
    (rowsAddresslink vc gc source link |> List.map (browseRow vc gc (browseValue vc gc)))
        |> propertyBox vc


rowsAddresslink : View.Config -> Graph.Config -> Address -> Link Address -> List (Row (Value Msg) Coords Msg)
rowsAddresslink vc gc source link =
    let
        currency =
            Id.currency source.id

        linkData =
            case link.link of
                Link.LinkData ld ->
                    Just ld

                Link.PlaceholderLinkData ->
                    Nothing
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
    , linkValueRow vc gc currency linkData
    ]


browseEntitylink : Plugins -> ModelState -> View.Config -> Graph.Config -> Entity -> Link Entity -> Html Msg
browseEntitylink plugins states vc gc source link =
    (rowsEntitylink vc gc source link |> List.map (browseRow vc gc (browseValue vc gc)))
        |> propertyBox vc


rowsEntitylink : View.Config -> Graph.Config -> Entity -> Link Entity -> List (Row (Value Msg) Coords Msg)
rowsEntitylink vc gc source link =
    let
        currency =
            Id.currency source.id

        linkData =
            case link.link of
                Link.LinkData ld ->
                    Just ld

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
                (.noTxs >> Locale.int vc.locale)
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
    , linkValueRow vc gc currency linkData
    ]


linkValueRow : View.Config -> Graph.Config -> String -> Maybe Link.LinkActualData -> Row (Value Msg) Coords Msg
linkValueRow vc gc parentCurrency linkData =
    Row
        ( if parentCurrency /= "eth" then
            "Estimated value"

          else
            "Value"
        , linkData
            |> Maybe.map
                (\ld -> Label.normalizeValues gc parentCurrency ld.value ld.tokenValues |> Value)
            |> Maybe.withDefault (String "")
        , Nothing
        )


browseAddresslinkTable : View.Config -> Graph.Config -> String -> AddresslinkTable -> Html Msg
browseAddresslinkTable vc gc coinCode table =
    case table of
        AddresslinkTxsUtxoTable t ->
            table_ vc cm (AddresslinkTxsUtxoTable.config vc coinCode) t

        AddresslinkTxsAccountTable t ->
            table_ vc cm (TxsAccountTable.config vc coinCode) t


multiValue : View.Config -> String -> String -> Api.Data.Values -> String
multiValue vc parentCoin coinCode v =
    if parentCoin == "eth" && vc.locale.currency /= Currency.Coin then
        Locale.currency vc.locale [ ( coinCode, v ) ]

    else
        Locale.currencyWithoutCode vc.locale [ ( coinCode, v ) ]


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


tableSeparator : View.Config -> Html msg
tableSeparator vc =
    div
        [ Css.tableSeparator vc |> css
        ]
        []
