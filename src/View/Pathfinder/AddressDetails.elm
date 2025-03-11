module View.Pathfinder.AddressDetails exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Config.Pathfinder as Pathfinder
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
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Init.Pathfinder.Id as Id
import Model.Currency exposing (asset, assetFromBase)
import Model.DateRangePicker as DateRangePicker
import Model.Locale as Locale
import Model.Pathfinder as Pathfinder exposing (getHavingTags, getSortedConceptsByWeight, getSortedLabelSummariesByRelevance)
import Model.Pathfinder.Address exposing (Address)
import Model.Pathfinder.AddressDetails as AddressDetails
import Model.Pathfinder.Colors as Colors
import Model.Pathfinder.Id as Id exposing (Id)
import Model.Pathfinder.Network as Network
import Model.Pathfinder.Table.RelatedAddressesTable exposing (ListType(..), getCurrentTable)
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Msg.Pathfinder as Pathfinder exposing (OverlayWindows(..))
import Msg.Pathfinder.AddressDetails as AddressDetails
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import RemoteData exposing (WebData)
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes exposing (css)
import Svg.Styled.Events as Svg
import Theme.Html.Buttons as Btns
import Theme.Html.Icons as HIcons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Theme.Svg.Icons as Icons
import Util.Css as Css
import Util.Data as Data
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags)
import Util.Tag as Tag
import Util.ThemedSelectBox as ThemedSelectBox
import Util.View exposing (copyIconPathfinder, loadingSpinner, none, onClickWithStop, timeToCell, truncateLongIdentifierWithLengths)
import View.Button exposing (btnDefaultConfig, primaryButton, secondaryButton)
import View.Locale as Locale
import View.Pathfinder.Address as Address
import View.Pathfinder.Details exposing (closeAttrs, dataTab, valuesToCell)
import View.Pathfinder.PagedTable as PagedTable
import View.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import View.Pathfinder.Table.TransactionTable as TransactionTable


view : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Html Pathfinder.Msg
view plugins pluginStates vc gc model id viewState =
    model.network.addresses
        |> Dict.get id
        |> Maybe.map
            (\address ->
                if Data.isAccountLike (Id.network id) then
                    account plugins pluginStates vc gc model id viewState address

                else
                    utxo plugins pluginStates vc gc model id viewState address
            )
        |> Maybe.withDefault none


utxo : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Address -> Html Pathfinder.Msg
utxo plugins pluginStates vc gc model id viewState address =
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
            }

        sidePanelAddressDetails =
            { clusterInfoVisible = Dict.member clstrId model.clusters
            , clusterInfoInstance =
                cluster
                    |> Maybe.withDefault RemoteData.NotAsked
                    |> RemoteData.unpack (\_ -> loadingSpinner vc Css.View.loadingSpinner)
                        (clusterInfoView vc model.config.isClusterDetailsOpen model.colors)
            }

        assetId =
            assetFromBase viewState.data.currency
    in
    SidePanelComponents.sidePanelAddressWithInstances
        (SidePanelComponents.sidePanelAddressAttributes
            |> Rs.s_sidePanelAddress
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelAddressDetails [ css fullWidth ]
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
                    []
                )
        )
        (SidePanelComponents.sidePanelAddressInstances
            |> setTags vc gc model id
            |> Rs.s_learnMore (Just none)
         -- |> Rs.s_iconsBinanceL
         --     (Just sidePanelData.actorIconInstance)
        )
        { pluginList = pluginList
        , pluginTagsList = pluginTagsList
        , relatedDataTabsList = relatedDataTabsList
        }
        { sidePanelAddress = sidePanelData
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
        , categoryTags = { tagLabel = "" }
        }


relatedAddressesDataTab : View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> WebData Api.Data.Entity -> Html AddressDetails.Msg
relatedAddressesDataTab vc model _ viewState cluster =
    let
        label =
            Locale.string vc.locale "Related addresses"

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
                        SidePanelComponents.sidePanelListHeaderTitle
                            { sidePanelListHeaderTitle =
                                { label = label
                                }
                            }
                    )
                    (\_ ->
                        SidePanelComponents.sidePanelListHeaderTitleWithNumber
                            { sidePanelListHeaderTitleWithNumber =
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

                            conf =
                                { optionToLabel =
                                    \a ->
                                        case a of
                                            TaggedAddresses ->
                                                Locale.string vc.locale "Tagged cluster addresses"

                                            ClusterAddresses ->
                                                Locale.string vc.locale "All cluster addresses"
                                }
                        in
                        div
                            [ css <|
                                SidePanelComponents.sidePanelRelatedAddressesContent_details.styles
                                    ++ fullWidth
                            ]
                            [ ThemedSelectBox.view conf ra.selectBox ra.selected
                                |> Html.map AddressDetails.SelectBoxMsg
                                |> List.singleton
                                |> div
                                    [ css SidePanelComponents.sidePanelRelatedAddressesContentSidePanelAddListFilterRow_details.styles ]
                            , PagedTable.pagedTableView vc
                                [ css fullWidth ]
                                (RelatedAddressesTable.config Css.Table.styles vc ratc ra)
                                (getCurrentTable ra)
                                AddressDetails.RelatedAddressesTablePagedTableMsg
                            ]
                            |> Just
        , onClick = AddressDetails.UserClickedToggleRelatedAddressesTable
        }


clusterInfoView : View.Config -> Bool -> Colors.ScopedColorAssignment -> Api.Data.Entity -> Html Pathfinder.Msg
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
                , onClick Pathfinder.UserClickedToggleClusterDetailsOpen
                ]

            label =
                Locale.string vc.locale "Cluster information"

            assetId =
                assetFromBase clstr.currency
        in
        if open then
            SidePanelComponents.clusterInformationOpenWithAttributes
                (SidePanelComponents.clusterInformationOpenAttributes
                    |> Rs.s_clusterInformationOpen headerAttr
                    |> Rs.s_ellipse25 clusterColor
                )
                { clusterInformationOpen = { label = label }
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
                    |> Rs.s_clusterInformationClosed headerAttr
                )
                { clusterInformationClosed = { label = label }
                }


dateRangePickerSelectionView : View.Config -> Maybe (DateRangePicker.Model AddressDetails.Msg) -> Html AddressDetails.Msg
dateRangePickerSelectionView vc model =
    let
        startP =
            Maybe.map .fromDate model
                |> Maybe.map
                    (Locale.posixToTimestampSeconds
                        >> Locale.timestampDateUniform vc.locale
                    )
                |> Maybe.withDefault ""

        endP =
            Maybe.map .toDate model
                |> Maybe.map
                    (Locale.posixToTimestampSeconds
                        >> Locale.timestampDateUniform vc.locale
                    )
                |> Maybe.withDefault ""

        open =
            [ onClick AddressDetails.OpenDateRangePicker
            , css [ Css.cursor Css.pointer ]
            ]
    in
    SidePanelComponents.sidePanelListFilterRowWithInstances
        (SidePanelComponents.sidePanelListFilterRowAttributes
            |> Rs.s_sidePanelListFilterRow [ css fullWidth ]
            |> Rs.s_iconsCloseBlack
                [ onClickWithStop AddressDetails.ResetDateRangePicker
                , css [ Css.cursor Css.pointer ]
                ]
            |> Rs.s_framedIcon open
            |> Rs.s_timePicker open
        )
        (SidePanelComponents.sidePanelListFilterRowInstances
            |> Rs.s_timePicker
                (model
                    |> Maybe.map (\_ -> Nothing)
                    |> Maybe.withDefault (Just none)
                )
        )
        { framedIcon =
            { iconInstance =
                HIcons.iconsFilter {}
            }
        , timePicker =
            { from = startP
            , to = endP
            , pronoun = Locale.string vc.locale "to"
            }
        }


transactionTableView : View.Config -> Id -> (Id -> Bool) -> TransactionTable.Model -> Html AddressDetails.Msg
transactionTableView vc addressId txOnGraphFn model =
    let
        styles =
            Css.Table.styles

        table =
            PagedTable.pagedTableView vc
                []
                (TransactionTable.config styles vc addressId txOnGraphFn)
                model.table
                AddressDetails.TransactionsTablePagedTableMsg
    in
    (case model.dateRangePicker of
        Just drp ->
            if DatePicker.isOpen drp.dateRangePicker then
                [ DateTimePicker.stylesheet
                , div [ css [ Css.fontSize (Css.px 12) ] ]
                    [ DatePicker.view drp.settings drp.dateRangePicker
                        |> Html.fromUnstyled
                    ]
                , div
                    [ SidePanelComponents.sidePanelListFilterRow_details.styles
                        ++ [ Css.justifyContent Css.flexEnd
                           , Css.property "gap" "10px"
                           ]
                        ++ fullWidth
                        |> css
                    ]
                    [ secondaryButton vc
                        (btnDefaultConfig
                            |> Rs.s_text "Reset"
                            |> Rs.s_onClick (Just AddressDetails.ResetDateRangePicker)
                        )
                    , primaryButton vc
                        (btnDefaultConfig
                            |> Rs.s_text "Apply filter"
                            |> Rs.s_onClick (Just AddressDetails.CloseDateRangePicker)
                        )
                    ]
                ]

            else
                [ Just drp
                    |> dateRangePickerSelectionView vc
                , table
                ]

        Nothing ->
            [ dateRangePickerSelectionView vc Nothing
            , table
            ]
    )
        |> div [ css [ Css.width (Css.pct 100) ] ]


transactionsDataTab : View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Html AddressDetails.Msg
transactionsDataTab vc model id viewState =
    let
        txOnGraphFn =
            \txId -> Dict.member txId model.network.txs
    in
    dataTab
        { title =
            SidePanelComponents.sidePanelListHeaderTitleTransactions
                { sidePanelListHeaderTitleTransactions =
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


account : Plugins -> ModelState -> View.Config -> Pathfinder.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Address -> Html Pathfinder.Msg
account plugins pluginStates vc gc model id viewState address =
    let
        fiatCurr =
            vc.preferredFiatCurrency

        fiatSum =
            viewState.data.tokenBalances |> Maybe.withDefault Dict.empty |> Dict.toList |> List.filterMap (Tuple.second >> Locale.getFiatValue fiatCurr) |> List.sum

        valueSumString =
            Locale.fiat vc.locale fiatCurr fiatSum

        toTokenRow _ ( symbol, values ) =
            let
                ass =
                    asset viewState.data.currency symbol

                value =
                    Locale.coin vc.locale ass values.value

                fvalue =
                    Locale.getFiatValue fiatCurr values
            in
            SidePanelComponents.tokenRowStateNeutralWithAttributes
                (SidePanelComponents.tokenRowStateNeutralAttributes
                    |> Rs.s_stateNeutral [ [ Css.hover SidePanelComponents.tokenRowStateHighlight_details.styles ] |> css ]
                )
                { stateNeutral =
                    { fiatValue = fvalue |> Maybe.map (Locale.fiat vc.locale fiatCurr) |> Maybe.withDefault ""
                    , tokenCode = ""
                    , tokenName = String.toUpper symbol
                    , tokenValue = value
                    }
                }

        attrClickSelect =
            if ntokens > 0 then
                [ Svg.onClick
                    (AddressDetails.UserClickedToggleTokenBalancesSelect
                        |> Pathfinder.AddressDetailsMsg viewState.addressId
                    )
                , [ Css.cursor Css.pointer ] |> css
                ]

            else
                [ [ Css.cursor Css.notAllowed ] |> css ]

        tokenRows =
            viewState.data.tokenBalances
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.indexedMap toTokenRow

        ntokens =
            viewState.data.tokenBalances |> Maybe.withDefault Dict.empty |> Dict.size

        ntokensString =
            "(" ++ (ntokens |> String.fromInt) ++ " tokens)"

        tokensDropDownOpen =
            SidePanelComponents.tokensDropDownOpenWithAttributes
                (SidePanelComponents.tokensDropDownOpenAttributes
                    |> Rs.s_tokensDropDownHeaderOpen attrClickSelect
                    |> Rs.s_tokensList
                        [ [ Css.position Css.absolute
                          , Css.zIndex (Css.int (Css.zIndexMainValue + 1))
                          , Css.top (Css.px SidePanelComponents.tokensDropDownClosed_details.height)
                          , Css.width (Css.px SidePanelComponents.tokensDropDownOpen_details.width)
                          ]
                            |> css
                        ]
                )
                { tokensList = tokenRows }
                { tokensDropDownHeaderOpen =
                    { numberOfToken = ntokensString, totalTokenValue = valueSumString }
                }

        tokensDropDownClosed =
            SidePanelComponents.tokensDropDownClosedWithInstances
                (SidePanelComponents.tokensDropDownClosedAttributes
                    |> Rs.s_tokensDropDownClosed attrClickSelect
                )
                SidePanelComponents.tokensDropDownClosedInstances
                { tokensDropDownClosed = { numberOfToken = ntokensString, totalTokenValue = valueSumString } }

        tokensDropdown =
            if viewState.tokenBalancesOpen then
                tokensDropDownOpen

            else
                tokensDropDownClosed

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
                            Locale.string vc.locale "address"
                       )
            }

        assetId =
            assetFromBase viewState.data.currency
    in
    SidePanelComponents.sidePanelEthAddressWithInstances
        (SidePanelComponents.sidePanelEthAddressAttributes
            |> Rs.s_sidePanelEthAddress
                [ sidePanelCss
                    |> css
                ]
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
                    []
                )
            |> Rs.s_pluginTagsList
                (if List.isEmpty pluginTagsList then
                    [ css [ Css.display Css.none ] ]

                 else
                    []
                )
        )
        (SidePanelComponents.sidePanelEthAddressInstances
            |> setTags vc gc model id
            |> Rs.s_tokensDropDownClosed (Just tokensDropdown)
            |> Rs.s_learnMore (Just none)
         -- |> Rs.s_iconsBinanceL
         --     (Just sidePanelData.actorIconInstance)
        )
        { pluginList = pluginList
        , pluginTagsList = pluginTagsList
        , relatedDataTabsList =
            [ transactionsDataTab vc model id viewState
                |> Html.map (Pathfinder.AddressDetailsMsg viewState.addressId)
            ]
        }
        { identifierWithCopyIcon = sidePanelAddressCopyIcon vc id
        , iconsTagL = { variant = HIcons.iconsTagLTypeDirect {} }
        , leftTab = { variant = none }
        , rightTab = { variant = none }
        , sidePanelAddressHeader = sidePanelAddressHeader
        , sidePanelEthAddress = sidePanelData
        , sidePanelEthAddressDetails =
            { clusterInfoVisible = False
            , clusterInfoInstance = none
            }
        , sidePanelRowWithDropdown = { valueCellInstance = none }
        , tokensDropDownClosed = { numberOfToken = ntokensString, totalTokenValue = valueSumString }
        , titleOfEthBalance = { infoLabel = Locale.string vc.locale "Balance" ++ " " ++ String.toUpper viewState.data.currency }
        , titleOfSidePanelRowWithDropdown = { infoLabel = Locale.string vc.locale "Token holdings" }
        , valueOfEthBalance = valuesToCell vc assetId viewState.data.balance
        , titleOfTotalReceived = { infoLabel = Locale.string vc.locale "Total received" }
        , valueOfTotalReceived = valuesToCell vc assetId viewState.data.totalReceived
        , titleOfTotalSent = { infoLabel = Locale.string vc.locale "Total sent" }
        , valueOfTotalSent = valuesToCell vc assetId viewState.data.totalSpent
        , titleOfLastUsage = { infoLabel = Locale.string vc.locale "Last usage" }
        , valueOfLastUsage = timeToCell vc viewState.data.lastTx.timestamp
        , titleOfFirstUsage = { infoLabel = Locale.string vc.locale "First usage" }
        , valueOfFirstUsage = timeToCell vc viewState.data.firstTx.timestamp

        -- , learnMoreButton = { variant = none }
        , categoryTags = { tagLabel = "" }
        }


viewLabelOfTags : View.Config -> Pathfinder.Config -> Pathfinder.Model -> Id -> Html Pathfinder.Msg
viewLabelOfTags vc gc model id =
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
                if gc.displayAllTagsInDetails then
                    lenTagLabels

                else
                    nMaxTags

            tagsControl =
                if lenTagLabels > nMaxTags then
                    if gc.displayAllTagsInDetails then
                        Html.span [ Css.tagLinkButtonStyle vc |> css, HA.title (Locale.string vc.locale "show less..."), Svg.onClick Pathfinder.UserClickedToggleDisplayAllTagsInDetails ]
                            [ Html.text (Locale.string vc.locale "less...") ]

                    else
                        Html.span [ Css.tagLinkButtonStyle vc |> css, HA.title (Locale.string vc.locale "show more..."), Svg.onClick Pathfinder.UserClickedToggleDisplayAllTagsInDetails ]
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
    Btns.buttonTypeTextStateRegularStyleTextWithAttributes
        (Btns.buttonTypeTextStateRegularStyleTextAttributes
            |> Rs.s_button
                [ [ Css.cursor Css.pointer ] |> css
                , onClick (Pathfinder.UserOpensDialogWindow (TagsList id))
                ]
        )
        { typeTextStateRegularStyleText =
            { buttonText = Locale.string vc.locale "Learn more"
            , iconInstance = none
            , iconVisible = False
            }
        }


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
            |> Maybe.withDefault (Icons.iconsAssignSvg [] {})
    , tabsVisible = False
    , tagSectionVisible = showExchangeTag || showOtherTag || pluginTagsVisible
    , pluginSTagVisible = pluginTagsVisible
    , actorVisible = showExchangeTag
    , tagsVisible = showOtherTag
    }


setTags : View.Config -> Pathfinder.Config -> Pathfinder.Model -> Id -> { a | categoryTags : Maybe (Html Pathfinder.Msg), labelOfActor : Maybe (Html Pathfinder.Msg) } -> { a | categoryTags : Maybe (Html Pathfinder.Msg), labelOfActor : Maybe (Html Pathfinder.Msg) }
setTags vc gc model id =
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
            viewLabelOfTags vc gc model id

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


sidePanelAddressCopyIcon : View.Config -> Id -> { identifier : String, copyIconInstance : Html msg, chevronInstance : Html a }
sidePanelAddressCopyIcon vc id =
    { identifier = Id.id id |> truncateLongIdentifierWithLengths 8 4
    , copyIconInstance = Id.id id |> copyIconPathfinder vc
    , chevronInstance = none
    }
