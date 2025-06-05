module View.Pathfinder.AddressDetails exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Config.View as View
import Css
import Css.DateTimePicker as DateTimePicker
import Css.Pathfinder as Css exposing (fullWidth, sidePanelCss)
import Css.Table
import Css.View
import Dict exposing (Dict)
import DurationDatePicker as DatePicker
import Html.Styled as Html exposing (Html, div, img)
import Html.Styled.Attributes as HA exposing (src)
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave, preventDefaultOn, stopPropagationOn)
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Currency exposing (asset, assetFromBase)
import Model.DateRangePicker as DateRangePicker
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords
import Model.Locale as Locale
import Model.Pathfinder as Pathfinder exposing (getHavingTags, getSortedConceptsByWeight, getSortedLabelSummariesByRelevance)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.ContextMenu as ContextMenu
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network
import Model.Pathfinder.Table.RelatedAddressesTable exposing (getTable)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder as Pathfinder exposing (OverlayWindows(..))
import Msg.Pathfinder.AddressDetails as AddressDetails
import PagedTable
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import RemoteData exposing (WebData)
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes exposing (css)
import Svg.Styled.Events as Svg
import Theme.Colors
import Theme.Html.Icons as HIcons
import Theme.Html.SelectionControls as SC
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util.Css exposing (spread)
import Util.Data as Data
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Graph exposing (decodeCoords)
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags)
import Util.Tag as Tag
import Util.ThemedSelectBox as ThemedSelectBox
import Util.View exposing (copyIconPathfinder, loadingSpinner, none, timeToCell, truncateLongIdentifierWithLengths)
import View.Button as Button
import View.Locale as Locale
import View.Pathfinder.Address as Address
import View.Pathfinder.Details exposing (closeAttrs, dataTab, valuesToCell)
import View.Pathfinder.PagedTable as PagedTable
import View.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import View.Pathfinder.Table.TransactionTable as TransactionTable


view : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Html Pathfinder.Msg
view plugins pluginStates vc model id viewState =
    div []
        [ model.network.addresses
            |> Dict.get id
            |> Maybe.map
                (\address ->
                    if Data.isAccountLike (Id.network id) then
                        account plugins pluginStates vc model id viewState address

                    else
                        utxo plugins pluginStates vc model id viewState address
                )
            |> Maybe.withDefault none
        , if viewState.txs.isTxFilterViewOpen then
            div
                [ [ Css.position Css.fixed
                  , Css.right (Css.px 42)
                  , Css.top (Css.pct 50)
                  , Css.property "transform" "translate(0%, -50%)"
                  , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1000))
                  ]
                    |> css
                ]
                [ txFilterView vc (Id.network id) viewState.txs |> Html.map (Pathfinder.AddressDetailsMsg viewState.addressId) ]

          else
            none
        ]


utxo : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Address -> Html Pathfinder.Msg
utxo plugins pluginStates vc model id viewState address =
    let
        pluginTagsVisible =
            List.length pluginTagsList > 0

        sidePanelData =
            makeSidePanelData model id pluginTagsVisible

        pluginList =
            Plugin.addressSidePanelHeader plugins pluginStates vc address

        pluginTagsList =
            Plugin.addressSidePanelHeaderTags plugins pluginStates vc address

        cluster =
            Dict.get clstrId model.clusters

        relatedDataTabsList =
            transactionsDataTab vc model id viewState
                :: (cluster
                        |> Maybe.map
                            (relatedAddressesDataTab vc model id viewState
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
                   )
                |> List.map (Html.map (Pathfinder.AddressDetailsMsg viewState.addressId))

        clstrId =
            Id.initClusterId viewState.data.currency viewState.data.entity

        sidePanelAddressHeader =
            { iconInstance =
                cluster
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Address.toNodeIconHtml address
            , headerText =
                (String.toUpper <| Id.network id)
                    ++ " "
                    ++ Locale.string vc.locale "address"
                    |> Locale.titleCase vc.locale
            }

        sidePanelAddressDetails =
            { clusterInfoVisible = Dict.member clstrId model.clusters
            , clusterInfoInstance =
                cluster
                    |> Maybe.withDefault RemoteData.NotAsked
                    |> RemoteData.unpack (\_ -> loadingSpinner vc Css.View.loadingSpinner)
                        (clusterInfoView vc viewState.isClusterDetailsOpen model.colors
                            >> Html.map (Pathfinder.AddressDetailsMsg id)
                        )
            }

        assetId =
            assetFromBase viewState.data.currency
    in
    SidePanelComponents.sidePanelAddressWithInstances
        (SidePanelComponents.sidePanelAddressAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelAddressDetails [ css fullWidth ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack closeAttrs
            |> Rs.s_pluginList [ css [ Css.display Css.none ] ]
            |> Rs.s_learnMore [ css [ Css.display Css.none ] ]
            |> Rs.s_tagsLayout
                (if sidePanelData.actorVisible || sidePanelData.tagsVisible then
                    []

                 else
                    [ css [ Css.display Css.none ] ]
                )
            |> Rs.s_pluginList
                (if List.isEmpty pluginList then
                    [ css [ Css.display Css.none ] ]

                 else
                    [ css [ Css.flexDirection Css.row, Css.justifyContent Css.spaceBetween ] ]
                )
        )
        (SidePanelComponents.sidePanelAddressInstances
            |> setTags vc viewState model id
            |> Rs.s_learnMore (Just none)
         -- |> Rs.s_iconsBinanceL
         --     (Just sidePanelData.actorIconInstance)
        )
        { pluginList = pluginList
        , pluginTagsList = pluginTagsList
        , relatedDataTabsList = relatedDataTabsList
        }
        { root = sidePanelData
        , iconsTagL = { variant = HIcons.iconsTagLTypeDirect {} }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , identifierWithCopyIcon = sidePanelAddressCopyIcon vc id
        , sidePanelAddressDetails = sidePanelAddressDetails
        , sidePanelAddressHeader = sidePanelAddressHeader
        , titleOfBalance = { infoLabel = Locale.string vc.locale "Balance" }
        , valueOfBalance = valuesToCell vc assetId viewState.data.balance
        , titleOfTotalReceived = { infoLabel = Locale.string vc.locale "Total received" }
        , valueOfTotalReceived = valuesToCell vc assetId viewState.data.totalReceived
        , titleOfTotalSent = { infoLabel = Locale.string vc.locale "Total sent" }
        , valueOfTotalSent = valuesToCell vc assetId viewState.data.totalSpent
        , titleOfLastUsage = { infoLabel = Locale.string vc.locale "Last usage" }
        , valueOfLastUsage = timeToCell vc viewState.data.lastTx.timestamp
        , titleOfFirstUsage = { infoLabel = Locale.string vc.locale "First usage" }
        , valueOfFirstUsage = timeToCell vc viewState.data.firstTx.timestamp

        -- , learnMoreButton = { variant = none }
        , categoryTags = { tagLabel = "", closeVisible = False }
        }


relatedAddressesDataTab : View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> WebData Api.Data.Entity -> Html AddressDetails.Msg
relatedAddressesDataTab vc model _ viewState cluster =
    let
        label =
            Locale.string vc.locale "Related addresses"
                |> Locale.titleCase vc.locale

        noRelatedAddresses =
            cluster
                |> RemoteData.map (.noAddresses >> flip (-) 1)
                |> RemoteData.withDefault 0
    in
    dataTab
        { title =
            cluster
                |> RemoteData.unpack
                    (\_ ->
                        SidePanelComponents.sidePanelListHeaderTitleWithAttributes
                            (SidePanelComponents.sidePanelListHeaderTitleAttributes
                                |> Rs.s_root [ spread ]
                            )
                            { root =
                                { label = label
                                }
                            }
                    )
                    (\_ ->
                        SidePanelComponents.sidePanelListHeaderTitleWithNumberWithAttributes
                            (SidePanelComponents.sidePanelListHeaderTitleWithNumberAttributes
                                |> Rs.s_root [ spread ]
                            )
                            { root =
                                { label = label
                                , number = Locale.int vc.locale noRelatedAddresses
                                }
                            }
                    )
        , content =
            if not viewState.relatedAddressesTableOpen || noRelatedAddresses == 0 then
                Nothing

            else
                case viewState.relatedAddresses of
                    RemoteData.Failure _ ->
                        Html.text "error" |> Just

                    RemoteData.Loading ->
                        loadingSpinner vc Css.View.loadingSpinner
                            |> Just

                    RemoteData.NotAsked ->
                        loadingSpinner vc Css.View.loadingSpinner
                            |> Just

                    RemoteData.Success ra ->
                        let
                            ratc =
                                { isChecked = flip Network.hasAddress model.network
                                , hasTags = getHavingTags model
                                , coinCode = assetFromBase viewState.data.currency
                                }
                        in
                        div
                            [ css <|
                                SidePanelComponents.sidePanelRelatedAddressesContent_details.styles
                                    ++ fullWidth
                            ]
                            [ PagedTable.pagedTableView vc
                                [ css fullWidth ]
                                (RelatedAddressesTable.config Css.Table.styles vc ratc ra)
                                (getTable ra)
                                AddressDetails.RelatedAddressesTablePagedTableMsg
                            ]
                            |> Just
        , onClick = AddressDetails.UserClickedToggleRelatedAddressesTable
        }


clusterInfoView : View.Config -> Bool -> Colors.ScopedColorAssignment -> Api.Data.Entity -> Html AddressDetails.Msg
clusterInfoView vc open colors clstr =
    if clstr.noAddresses <= 1 then
        none

    else
        let
            clstrid =
                Id.initClusterId clstr.currency clstr.entity

            clusterColor =
                Colors.getAssignedColor Colors.Clusters clstrid colors
                    |> Maybe.map (.color >> Util.View.toCssColor)
                    |> Maybe.withDefault (Css.rgba 0 0 0 0)
                    |> Css.fill
                    |> Css.important
                    |> List.singleton
                    |> css
                    |> List.singleton

            headerAttr =
                [ Css.cursor Css.pointer
                    :: fullWidth
                    |> css
                , onClick AddressDetails.UserClickedToggleClusterDetailsOpen
                ]

            label =
                Locale.string vc.locale "Cluster information"

            assetId =
                assetFromBase clstr.currency
        in
        if open then
            SidePanelComponents.clusterInformationOpenWithAttributes
                (SidePanelComponents.clusterInformationOpenAttributes
                    |> Rs.s_root headerAttr
                    |> Rs.s_ellipse25 clusterColor
                )
                { root = { label = label }
                , titleOfClusterId = { infoLabel = Locale.string vc.locale "Cluster" }
                , valueOfClusterId = { label = String.fromInt clstr.entity }
                , titleOfNumberOfAddresses = { infoLabel = Locale.string vc.locale "Number of addresses" }
                , valueOfNumberOfAddresses =
                    { firstRowText = String.fromInt clstr.noAddresses
                    , secondRowText = ""
                    , secondRowVisible = False
                    }
                , sidePanelRowCustomValueCell = { valueCell = none }
                , titleOfSidePanelRowCustomValueCell = { infoLabel = "" }
                , titleOfBalance = { infoLabel = Locale.string vc.locale "Balance" }
                , valueOfBalance = valuesToCell vc assetId clstr.balance
                , titleOfTotalReceived = { infoLabel = Locale.string vc.locale "Total received" }
                , valueOfTotalReceived = valuesToCell vc assetId clstr.totalReceived
                , titleOfTotalSent = { infoLabel = Locale.string vc.locale "Total sent" }
                , valueOfTotalSent = valuesToCell vc assetId clstr.totalSpent
                , titleOfLastUsage = { infoLabel = Locale.string vc.locale "Last usage" }
                , valueOfLastUsage = timeToCell vc clstr.lastTx.timestamp
                , titleOfFirstUsage = { infoLabel = Locale.string vc.locale "First usage" }
                , valueOfFirstUsage = timeToCell vc clstr.firstTx.timestamp
                }

        else
            SidePanelComponents.clusterInformationClosedWithAttributes
                (SidePanelComponents.clusterInformationClosedAttributes
                    |> Rs.s_root headerAttr
                )
                { root = { label = label }
                }


transactionTableView : View.Config -> Id -> (Id -> Bool) -> TransactionTable.Model -> Html AddressDetails.Msg
transactionTableView vc addressId txOnGraphFn model =
    let
        styles =
            Css.Table.styles

        allChecked =
            model.table
                |> PagedTable.getPage
                |> List.map Tx.getTxIdForAddressTx
                |> List.all txOnGraphFn

        table =
            PagedTable.pagedTableView vc
                []
                (TransactionTable.config styles vc addressId txOnGraphFn allChecked)
                model.table
                AddressDetails.TransactionsTablePagedTableMsg
    in
    [ filterHeader vc model
    , table
    ]
        |> div [ css [ Css.width (Css.pct 100) ] ]


closeButtonGrey : AddressDetails.Msg -> Html AddressDetails.Msg
closeButtonGrey msg =
    HIcons.iconsCloseBlackWithAttributes
        (HIcons.iconsCloseBlackAttributes
            |> Rs.s_root
                [ [ Util.Css.overrideBlack Theme.Colors.greyBlue500 ] |> css
                , Util.View.pointer
                , onClick msg
                ]
        )
        {}


dateTimeFilterHeader : View.Config -> DateRangePicker.Model AddressDetails.Msg -> Html AddressDetails.Msg
dateTimeFilterHeader vc dmodel =
    let
        startP =
            dmodel.fromDate
                |> Locale.posixToTimestampSeconds
                |> Locale.timestampDateUniform vc.locale

        endP =
            dmodel.toDate
                |> Locale.posixToTimestampSeconds
                |> Locale.timestampDateUniform vc.locale
    in
    SidePanelComponents.filterLabel
        { root =
            { iconInstance =
                closeButtonGrey AddressDetails.ResetDateRangePicker
            , text1 = endP
            , text2 = Locale.string vc.locale "to"
            , text3 = startP
            , dateRangeVisible = True
            }
        }


directionFilterHeader : View.Config -> Direction -> Html AddressDetails.Msg
directionFilterHeader vc dir =
    stringFilterHeader vc
        (case dir of
            Incoming ->
                "Incoming only"

            Outgoing ->
                "Outgoing only"
        )
        AddressDetails.ResetTxDirectionFilter


stringFilterHeader : View.Config -> String -> AddressDetails.Msg -> Html AddressDetails.Msg
stringFilterHeader vc str msg =
    SidePanelComponents.filterLabel
        { root =
            { iconInstance =
                closeButtonGrey msg
            , text3 = ""
            , text2 = ""
            , text1 =
                Locale.string vc.locale str
            , dateRangeVisible = False
            }
        }


assetFilterHeader : View.Config -> String -> Html AddressDetails.Msg
assetFilterHeader vc asset =
    stringFilterHeader vc asset AddressDetails.ResetTxAssetFilter


filterHeader : View.Config -> TransactionTable.Model -> Html AddressDetails.Msg
filterHeader vc model =
    div
        [ [ Css.displayFlex
          , Css.justifyContent Css.spaceBetween
          , Css.padding (Css.px 10)
          , Css.property "gap" "5px"
          ]
            |> css
        ]
        [ div
            [ [ Css.displayFlex
              , Css.flexDirection Css.row
              , Css.property "gap" "5px"
              , Css.flexWrap Css.wrap
              , Css.width (Css.px 320)
              ]
                |> css
            ]
            [ model.dateRangePicker |> Maybe.map (dateTimeFilterHeader vc) |> Maybe.withDefault none
            , model.direction |> Maybe.map (directionFilterHeader vc) |> Maybe.withDefault none
            , model.selectedAsset |> Maybe.map (assetFilterHeader vc) |> Maybe.withDefault none
            ]
        , div []
            [ HIcons.framedIconWithAttributes
                (HIcons.framedIconAttributes
                    |> Rs.s_root
                        [ onClick AddressDetails.ToggleTxFilterView
                        , Util.View.pointer
                        ]
                )
                { root = { iconInstance = HIcons.iconsFilter {} } }
            ]
        ]


txFilterView : View.Config -> String -> TransactionTable.Model -> Html AddressDetails.Msg
txFilterView vc net model =
    let
        toRadio name selected msg =
            SC.radioLabelWithAttributes
                (SC.radioLabelAttributes
                    |> Rs.s_root [ onClick msg, [ Css.cursor Css.pointer ] |> css ]
                )
                { radio =
                    { variant =
                        if selected then
                            SC.radioStateOnSizeSmall {}

                        else
                            SC.radioStateOffSizeSmall {}
                    }
                , root = { radioLabel = Locale.string vc.locale name }
                }

        isAssetFilterVisible =
            Data.isAccountLike net
    in
    SidePanelComponents.filterTransactionsPopupWithAttributes
        (SidePanelComponents.filterTransactionsPopupAttributes
            |> Rs.s_assetType
                (if isAssetFilterVisible then
                    []

                 else
                    [ Css.display Css.none ] |> css |> List.singleton
                )
            |> Rs.s_iconsCloseBlack [ Util.View.pointer, onClick AddressDetails.CloseTxFilterView ]
        )
        { radioItemsList =
            [ toRadio "All transactions" (model.direction == Nothing) AddressDetails.TxTableFilterShowAllTxs
            , toRadio "Incoming only" (model.direction == Just Incoming) AddressDetails.TxTableFilterShowIncomingTxOnly
            , toRadio "Outgoing only" (model.direction == Just Outgoing) AddressDetails.TxTableFilterShowOutgoingTxOnly
            ]
        }
        { assetType = { label = "" }
        , cancelButton =
            { variant =
                Button.defaultConfig
                    |> Rs.s_text "Reset"
                    |> Rs.s_onClick (Just AddressDetails.ResetAllTxFilters)
                    |> Button.secondaryButton vc
            }
        , confirmButton =
            { variant =
                Button.defaultConfig
                    |> Rs.s_text "Done"
                    |> Rs.s_onClick (Just AddressDetails.CloseTxFilterView)
                    |> Button.primaryButton vc
            }
        , dropDown =
            { variant =
                if isAssetFilterVisible then
                    ThemedSelectBox.viewWithLabel (ThemedSelectBox.defaultConfig (Maybe.withDefault "All assets") |> Rs.s_width (Just (Css.px 200))) model.assetSelectBox model.selectedAsset (Locale.string vc.locale "Asset Type")
                        |> Html.map AddressDetails.TxTableAssetSelectBoxMsg

                else
                    none
            }
        , root =
            { dateInstance =
                case model.dateRangePicker of
                    Just dmodel ->
                        let
                            startP =
                                dmodel.fromDate
                                    |> Locale.posixToTimestampSeconds
                                    |> Locale.timestampDateUniform vc.locale

                            endP =
                                dmodel.toDate
                                    |> Locale.posixToTimestampSeconds
                                    |> Locale.timestampDateUniform vc.locale
                        in
                        if DatePicker.isOpen dmodel.dateRangePicker then
                            div []
                                [ DateTimePicker.stylesheet
                                , div [ css [ Css.fontSize (Css.px 12) ] ]
                                    [ DatePicker.view dmodel.settings dmodel.dateRangePicker
                                        |> Html.fromUnstyled
                                    ]
                                ]

                        else
                            SidePanelComponents.datePickerFilledWithAttributes
                                (SidePanelComponents.datePickerFilledAttributes
                                    |> Rs.s_root
                                        [ onClick AddressDetails.OpenDateRangePicker
                                        , Util.View.pointer
                                        , [ Css.hover SidePanelComponents.datePickerFilledStateHover_details.styles ] |> css
                                        ]
                                )
                                { root = { from = startP, to = endP, pronoun = Locale.string vc.locale "to", state = SidePanelComponents.DatePickerFilledStateDefault } }

                    _ ->
                        SidePanelComponents.datePickerCtaWithAttributes
                            (SidePanelComponents.datePickerCtaAttributes
                                |> Rs.s_root
                                    [ onClick AddressDetails.OpenDateRangePicker
                                    , Util.View.pointer
                                    , [ Css.hover SidePanelComponents.datePickerCtaStateHover_details.styles ] |> css
                                    ]
                            )
                            { root =
                                { placeholder = Locale.string vc.locale "Select date range"
                                , state = SidePanelComponents.DatePickerCtaStateDefault
                                }
                            }
            , dateLabel = Locale.string vc.locale "Date Range"
            , headerTitle = Locale.string vc.locale "Transaction Filter"
            , txDirection = Locale.string vc.locale "Transaction Direction"
            }
        }


transactionsDataTab : View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Html AddressDetails.Msg
transactionsDataTab vc model id viewState =
    let
        txOnGraphFn =
            \txId -> Dict.member txId model.network.txs
    in
    dataTab
        { title =
            SidePanelComponents.sidePanelListHeaderTitleTransactionsWithAttributes
                (SidePanelComponents.sidePanelListHeaderTitleTransactionsAttributes
                    |> Rs.s_root [ spread ]
                )
                { root =
                    { totalNumber =
                        (viewState.data.noIncomingTxs + viewState.data.noOutgoingTxs)
                            |> Locale.int vc.locale
                    , incomingNumber =
                        viewState.data.noIncomingTxs
                            |> Locale.int vc.locale
                    , outgoingNumber =
                        viewState.data.noOutgoingTxs
                            |> Locale.int vc.locale
                    , title = Locale.string vc.locale "Transactions"
                    }
                }
        , content =
            if viewState.transactionsTableOpen then
                transactionTableView vc id txOnGraphFn viewState.txs
                    |> Just

            else
                Nothing
        , onClick = AddressDetails.UserClickedToggleTransactionTable
        }


accountValuesRundown : View.Config -> AddressDetails.Model -> { totalReceived : Html Pathfinder.Msg, totalSpent : Html Pathfinder.Msg, balance : Html Pathfinder.Msg }
accountValuesRundown vc viewState =
    let
        fiatCurr =
            vc.preferredFiatCurrency

        getValue ( symbol, values ) =
            let
                ass =
                    asset viewState.data.currency symbol

                value =
                    Locale.coinWithoutCode vc.locale ass values.value

                fvalue =
                    Locale.getFiatValue fiatCurr values
            in
            { fiat =
                fvalue
                    |> Maybe.map (Locale.fiat vc.locale fiatCurr)
                    |> Maybe.withDefault ""
            , fiatFloat = fvalue
            , native = value
            , asset = ass
            }

        compute ( open, msg ) title values valueToken =
            let
                fiatSumTotalTokens =
                    valueToken
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList
                        |> List.filterMap (Tuple.second >> Locale.getFiatValue fiatCurr)
                        |> List.sum

                fiatSumTotal =
                    fiatSumTotalTokens
                        + (values
                            |> Locale.getFiatValue fiatCurr
                            |> Maybe.withDefault 0.0
                          )

                nativeValue =
                    getValue ( viewState.data.currency, values )

                row inpt =
                    let
                        dotsLAttr =
                            [ [ Css.flexGrow (Css.int 1)
                              , Css.borderBottomStyle Css.dotted
                              ]
                                |> css
                            ]
                    in
                    SidePanelComponents.sidePanelRowChevronSubRowWithInstances
                        (SidePanelComponents.sidePanelRowChevronSubRowAttributes
                            |> Rs.s_root
                                [ [ Css.justifyContent Css.stretch
                                  , Css.width (Css.pct 100)
                                  ]
                                    |> css
                                ]
                            |> Rs.s_dotsLine dotsLAttr
                        )
                        SidePanelComponents.sidePanelRowChevronSubRowInstances
                        { root =
                            { coinLabel = inpt.asset.asset |> String.toUpper
                            , coinValue = inpt.native
                            , fiatValue = inpt.fiat
                            }
                        }

                fixedleftAttr =
                    [ [ Css.left (Css.px (SidePanelComponents.sidePanelRowChevronClosedIconGroup_details.x * 2))
                      , Css.alignItems Css.center |> Css.important
                      ]
                        |> css
                    ]

                fw =
                    [ Css.width (Css.pct 100) ]
                        |> css

                clickAttr =
                    [ onClick msg, fw, Util.View.pointer ]
            in
            if open then
                SidePanelComponents.sidePanelRowOpenWithAttributes
                    (SidePanelComponents.sidePanelRowOpenAttributes
                        |> Rs.s_root clickAttr
                        |> Rs.s_sidePanelRowChevronOpen [ fw ]
                        |> Rs.s_iconGroup fixedleftAttr
                        |> Rs.s_tokensList
                            [ css [ Css.overflowY Css.auto ] ]
                    )
                    { tokensList =
                        (nativeValue
                            :: (valueToken
                                    |> Maybe.withDefault Dict.empty
                                    |> Dict.toList
                                    |> List.map getValue
                               )
                        )
                            |> List.sortBy
                                (.fiatFloat
                                    >> Maybe.withDefault 0.0
                                )
                            |> List.reverse
                            |> List.map row
                    }
                    { sidePanelRowChevronOpen =
                        { iconInstance = HIcons.iconsChevronDownThin {}
                        , title = Locale.string vc.locale title
                        , value = Locale.fiat vc.locale fiatCurr fiatSumTotal
                        }
                    }

            else
                SidePanelComponents.sidePanelRowChevronClosedWithAttributes
                    (SidePanelComponents.sidePanelRowChevronClosedAttributes
                        |> Rs.s_root
                            clickAttr
                        |> Rs.s_iconGroup fixedleftAttr
                    )
                    { root =
                        { iconInstance = HIcons.iconsChevronRightThin { root = { state = HIcons.IconsChevronRightThinStateDefault } }
                        , title = Locale.string vc.locale title
                        , value = Locale.fiat vc.locale fiatCurr fiatSumTotal
                        }
                    }
    in
    { totalReceived =
        compute
            ( viewState.totalReceivedDetailsOpen
            , AddressDetails.UserClickedToggleTotalReceivedDetails
                |> Pathfinder.AddressDetailsMsg viewState.addressId
            )
            "Total received"
            viewState.data.totalReceived
            viewState.data.totalTokensReceived
    , totalSpent =
        compute
            ( viewState.totalSentDetailsOpen
            , AddressDetails.UserClickedToggleTotalSpentDetails
                |> Pathfinder.AddressDetailsMsg viewState.addressId
            )
            "Total sent"
            viewState.data.totalSpent
            viewState.data.totalTokensSpent
    , balance =
        compute
            ( viewState.balanceDetailsOpen
            , AddressDetails.UserClickedToggleBalanceDetails
                |> Pathfinder.AddressDetailsMsg viewState.addressId
            )
            "Balance"
            viewState.data.balance
            viewState.data.tokenBalances
    }


account : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Address -> Html Pathfinder.Msg
account plugins pluginStates vc model id viewState address =
    let
        pluginList =
            Plugin.addressSidePanelHeader plugins pluginStates vc address

        pluginTagsList =
            Plugin.addressSidePanelHeaderTags plugins pluginStates vc address

        pluginTagsVisible =
            List.length pluginTagsList > 0

        sidePanelData =
            makeSidePanelData model id pluginTagsVisible

        sidePanelAddressHeader =
            { iconInstance =
                Address.toNodeIconHtml address Nothing
            , headerText =
                (String.toUpper <| Id.network id)
                    ++ " "
                    ++ (if viewState.data.isContract |> Maybe.withDefault False then
                            Locale.string vc.locale "Smart Contract"

                        else
                            Locale.string vc.locale "Address"
                       )
            }

        accountValuesRundownHtml =
            accountValuesRundown vc viewState
    in
    SidePanelComponents.sidePanelEthAddressWithInstances
        (SidePanelComponents.sidePanelEthAddressAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack closeAttrs
            |> Rs.s_pluginList [ css [ Css.display Css.none ] ]
            |> Rs.s_learnMore [ css [ Css.display Css.none ] ]
            |> Rs.s_tagsLayout
                (if sidePanelData.actorVisible || sidePanelData.tagsVisible then
                    []

                 else
                    [ css [ Css.display Css.none ] ]
                )
            |> Rs.s_pluginList
                (if List.isEmpty pluginList then
                    [ css [ Css.display Css.none ] ]

                 else
                    [ css [ Css.flexDirection Css.row, Css.justifyContent Css.spaceBetween ] ]
                )
            |> Rs.s_pluginTagsList
                (if List.isEmpty pluginTagsList then
                    [ css [ Css.display Css.none ] ]

                 else
                    []
                )
        )
        (SidePanelComponents.sidePanelEthAddressInstances
            |> setTags vc viewState model id
            |> Rs.s_learnMore (Just none)
            |> Rs.s_totalReceivedRow (Just accountValuesRundownHtml.totalReceived)
            |> Rs.s_totalSentRow (Just accountValuesRundownHtml.totalSpent)
            |> Rs.s_balanceRow (Just accountValuesRundownHtml.balance)
        )
        { pluginList = pluginList
        , pluginTagsList = pluginTagsList
        , relatedDataTabsList =
            [ transactionsDataTab vc model id viewState
                |> Html.map (Pathfinder.AddressDetailsMsg viewState.addressId)
            ]
        , tokensList = []
        }
        { identifierWithCopyIcon = sidePanelAddressCopyIcon vc id
        , iconsTagL = { variant = HIcons.iconsTagLTypeDirect {} }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , sidePanelAddressHeader = sidePanelAddressHeader
        , root = sidePanelData
        , sidePanelEthAddressDetails =
            { clusterInfoVisible = False
            , clusterInfoInstance = none
            }
        , titleOfLastUsage = { infoLabel = Locale.string vc.locale "Last usage" }
        , valueOfLastUsage = timeToCell vc viewState.data.lastTx.timestamp
        , titleOfFirstUsage = { infoLabel = Locale.string vc.locale "First usage" }
        , valueOfFirstUsage = timeToCell vc viewState.data.firstTx.timestamp
        , categoryTags = { tagLabel = "", closeVisible = False }
        , balanceRow = { iconInstance = none, title = "", value = "" }
        , totalSentRow = { iconInstance = none, title = "", value = "" }
        , sidePanelRowChevronOpen = { iconInstance = none, title = "", value = "" }
        }


viewLabelOfTags : View.Config -> AddressDetails.Model -> Pathfinder.Model -> Id -> Html Pathfinder.Msg
viewLabelOfTags vc viewState model id =
    let
        ts =
            getTagSummary model id
    in
    if vc.showLabelsInTaggingOverview then
        let
            showTag i ( tid, t ) =
                let
                    ctx =
                        { context = tid, domId = tid }
                in
                Html.div
                    [ onMouseEnter (Pathfinder.UserMovesMouseOverTagLabel ctx)
                    , onMouseLeave (Pathfinder.UserMovesMouseOutTagLabel ctx)
                    , HA.css SidePanelComponents.sidePanelAddressSidePanelHeaderTags_details.styles
                    , HA.id ctx.domId
                    , css [ Css.cursor Css.pointer ]
                    , onClick (Pathfinder.UserOpensDialogWindow (TagsList id))
                    ]
                    (Html.text t.label
                        :: (if i < (lenTagLabels - 1) then
                                [ Html.text "," ]

                            else
                                []
                           )
                    )

            nMaxTags =
                3

            tagLabels =
                ts
                    |> Maybe.map
                        (\x ->
                            if hasOnlyExchangeTags x then
                                []

                            else
                                getSortedLabelSummariesByRelevance x
                        )
                    |> Maybe.withDefault []

            lenTagLabels =
                List.length tagLabels

            nTagsToShow =
                if viewState.displayAllTagsInDetails then
                    lenTagLabels

                else
                    nMaxTags

            tagsControl =
                if lenTagLabels > nMaxTags then
                    if viewState.displayAllTagsInDetails then
                        Html.span
                            [ Css.tagLinkButtonStyle vc |> css
                            , HA.title (Locale.string vc.locale "show less...")
                            , AddressDetails.UserClickedToggleDisplayAllTagsInDetails
                                |> Pathfinder.AddressDetailsMsg id
                                |> Svg.onClick
                            ]
                            [ Html.text (Locale.string vc.locale "less...") ]

                    else
                        Html.span
                            [ Css.tagLinkButtonStyle vc |> css
                            , HA.title (Locale.string vc.locale "show more...")
                            , AddressDetails.UserClickedToggleDisplayAllTagsInDetails
                                |> Pathfinder.AddressDetailsMsg id
                                |> Svg.onClick
                            ]
                            [ Html.text ("+" ++ String.fromInt (lenTagLabels - nMaxTags) ++ " "), Html.text (Locale.string vc.locale "more...") ]

                else
                    none
        in
        div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.flexWrap Css.wrap
                , Css.property "gap" "1ex"
                , Css.alignItems Css.center
                , Css.width <| Css.px (SidePanelComponents.sidePanelAddress_details.width * 0.8)
                ]
            ]
            ((tagLabels |> List.take nTagsToShow |> List.indexedMap showTag) ++ [ tagsControl ])

    else
        let
            concepts =
                ts
                    |> Maybe.map getSortedConceptsByWeight
                    |> Maybe.withDefault []
        in
        div
            [ css
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.flexWrap Css.wrap
                , Css.property "gap" "1ex"
                , Css.alignItems Css.center
                , Css.width <| Css.px (SidePanelComponents.sidePanelAddress_details.width * 0.8)
                ]
            ]
            ((concepts
                |> List.map
                    (Tag.conceptItem vc id
                        >> Html.map AddressDetails.TooltipMsg
                        >> Html.map (Pathfinder.AddressDetailsMsg id)
                    )
             )
                ++ [ learnMoreButton vc id ]
            )


learnMoreButton : View.Config -> Id -> Html Pathfinder.Msg
learnMoreButton vc id =
    Button.defaultConfig
        |> Rs.s_text "Learn more"
        |> Rs.s_onClick (Just (Pathfinder.UserOpensDialogWindow (TagsList id)))
        |> Button.linkButtonBlue vc


getTagSummary : { a | tagSummaries : Dict Id Pathfinder.HavingTags } -> Id -> Maybe Api.Data.TagSummary
getTagSummary model id =
    case Dict.get id model.tagSummaries of
        Just (Pathfinder.HasTagSummaries { withCluster }) ->
            Just withCluster

        Just (Pathfinder.HasTagSummaryWithCluster ts) ->
            Just ts

        Just (Pathfinder.HasTagSummaryOnlyWithCluster ts) ->
            Just ts

        _ ->
            Nothing


makeSidePanelData : Pathfinder.Model -> Id -> Bool -> { actorIconInstance : Svg msg, tabsVisible : Bool, tagSectionVisible : Bool, pluginSTagVisible : Bool, actorVisible : Bool, tagsVisible : Bool }
makeSidePanelData model id pluginTagsVisible =
    let
        ts =
            getTagSummary model id

        actor_id =
            ts |> Maybe.andThen .bestActor

        actor =
            actor_id
                |> Maybe.andThen (\i -> Dict.get i model.actors)

        actorImg =
            actor
                |> Maybe.andThen .context
                |> Maybe.andThen (.images >> List.head)
                |> Maybe.map addProtocolPrefx

        actorText =
            actor
                |> Maybe.map .label

        showExchangeTag =
            actorText /= Nothing

        nrTagsAddress =
            ts |> Maybe.map .tagCount |> Maybe.withDefault 0

        showOtherTag =
            nrTagsAddress > 0 && (ts |> Maybe.map (hasOnlyExchangeTags >> not) |> Maybe.withDefault True)
    in
    { actorIconInstance =
        actorImg
            |> Maybe.map
                (\imgSrc ->
                    let
                        iconDetails =
                            HIcons.iconsAssign_details
                    in
                    img
                        [ src imgSrc
                        , HA.alt <| Maybe.withDefault "" <| actorText
                        , HA.width <| round iconDetails.width
                        , HA.height <| round iconDetails.height
                        , HA.css iconDetails.styles
                        ]
                        []
                        |> List.singleton
                        |> div
                            [ HA.css iconDetails.styles
                            , HA.css
                                [ iconDetails.width
                                    |> Css.px
                                    |> Css.width
                                , iconDetails.height
                                    |> Css.px
                                    |> Css.height
                                ]
                            ]
                )
            |> Maybe.withDefault (HIcons.iconsAssign {})
    , tabsVisible = False
    , tagSectionVisible = showExchangeTag || showOtherTag || pluginTagsVisible
    , pluginSTagVisible = pluginTagsVisible
    , actorVisible = showExchangeTag
    , tagsVisible = showOtherTag
    }


setTags : View.Config -> AddressDetails.Model -> Pathfinder.Model -> Id -> { a | categoryTags : Maybe (Html Pathfinder.Msg), labelOfActor : Maybe (Html Pathfinder.Msg) } -> { a | categoryTags : Maybe (Html Pathfinder.Msg), labelOfActor : Maybe (Html Pathfinder.Msg) }
setTags vc viewState model id =
    let
        ts =
            getTagSummary model id

        actor_id =
            ts |> Maybe.andThen .bestActor

        actor =
            actor_id
                |> Maybe.andThen (\i -> Dict.get i model.actors)

        actorText =
            actor
                |> Maybe.map .label

        labelOfTags =
            viewLabelOfTags vc viewState model id

        labelOfActor =
            actor_id
                |> Maybe.map
                    (\aid ->
                        let
                            text =
                                actorText |> Maybe.withDefault ""

                            ctx =
                                { context = aid, domId = aid ++ "_actor" }
                        in
                        Html.div
                            [ HA.css
                                SidePanelComponents.sidePanelAddressTags_details.styles
                            ]
                            [ Html.div
                                [ css SidePanelComponents.sidePanelEthAddressLabelOfActor_details.styles
                                , onMouseEnter (Pathfinder.UserMovesMouseOverActorLabel ctx)
                                , onMouseLeave (Pathfinder.UserMovesMouseOutActorLabel ctx)
                                , css [ Css.cursor Css.default ]
                                , HA.id ctx.domId
                                ]
                                [ Html.text text
                                ]
                            , if Maybe.map hasOnlyExchangeTags ts == Just True then
                                learnMoreButton vc id

                              else
                                none
                            ]
                    )
    in
    Rs.s_categoryTags
        (Just labelOfTags)
        >> Rs.s_labelOfActor
            labelOfActor


sidePanelAddressCopyIcon : View.Config -> Id -> { identifier : String, copyIconInstance : Html Pathfinder.Msg, addTagIconInstance : Html Pathfinder.Msg, chevronInstance : Html Pathfinder.Msg }
sidePanelAddressCopyIcon vc id =
    { identifier = Id.id id |> truncateLongIdentifierWithLengths 8 4
    , copyIconInstance = Id.id id |> copyIconPathfinder vc
    , addTagIconInstance = none
    , chevronInstance =
        div [ stopPropagationOn "click" (Json.Decode.succeed ( Pathfinder.NoOp, True )) ]
            [ HIcons.iconsChevronDownThinWithAttributes
                (HIcons.iconsChevronDownThinAttributes
                    |> Rs.s_root
                        [ Util.View.pointer
                        , decodeCoords Coords.Coords
                            |> Json.Decode.map (\c -> ( Pathfinder.UserOpensContextMenu c (ContextMenu.AddressIdChevronActions id), True ))
                            |> preventDefaultOn "click"
                        ]
                )
                {}
            ]
    }
