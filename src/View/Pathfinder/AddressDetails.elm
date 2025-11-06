module View.Pathfinder.AddressDetails exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as Inf
import Components.PagedTable as PagedTable
import Config.Pathfinder exposing (TracingMode(..))
import Config.View as View
import Css
import Css.Pathfinder as Css exposing (fullWidth, sidePanelCss)
import Css.Table
import Css.View
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, img, span)
import Html.Styled.Attributes as HA exposing (src)
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave, preventDefaultOn, stopPropagationOn)
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Currency exposing (asset, assetFromBase)
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
import Model.Pathfinder.Table.RelatedAddressesPubkeyTable as RelatedAddressesPubkeyTable
import Model.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import Model.Pathfinder.Table.TransactionTable as TransactionTable
import Model.Pathfinder.Tx as Tx
import Msg.Pathfinder as Pathfinder exposing (Msg(..), OverlayWindows(..))
import Msg.Pathfinder.AddressDetails as AddressDetails exposing (RelatedAddressesTooltipMsgs(..), TooltipMsgs(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import RemoteData exposing (WebData)
import Sha256
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes exposing (css)
import Svg.Styled.Events as Svg
import Theme.Html.Icons as HIcons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Util exposing (allAndNotEmpty)
import Util.Css exposing (spread)
import Util.Data as Data exposing (isAccountLike)
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Graph exposing (decodeCoords)
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags)
import Util.Tag as Tag
import Util.ThemedSelectBox as ThemedSelectBox
import Util.View exposing (HintPosition(..), copyIconPathfinderAbove, emptyCell, fixFillRule, iconWithHint, loadingSpinner, none, timeToCell, truncateLongIdentifierWithLengths)
import View.Button as Button
import View.Locale as Locale
import View.Pathfinder.Address as Address
import View.Pathfinder.Details exposing (closeAttrs, dataTab, valuesToCell)
import View.Pathfinder.InfiniteTable as InfiniteTable
import View.Pathfinder.PagedTable as PagedTable
import View.Pathfinder.Table.NeighborAddressesTable as NeighborAddressesTable
import View.Pathfinder.Table.RelatedAddressesPubkeyTable as RelatedAddressesPubkeyTable
import View.Pathfinder.Table.RelatedAddressesTable as RelatedAddressesTable
import View.Pathfinder.Table.TransactionTable as TransactionTable
import View.Pathfinder.TransactionFilter as TransactionFilter


view : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Html Pathfinder.Msg
view plugins pluginStates vc model id viewState =
    let
        filterDialogMsgs =
            { closeTxFilterViewMsg = AddressDetails.CloseTxFilterView
            , txTableFilterShowAllTxsMsg = Just AddressDetails.TxTableFilterShowAllTxs
            , txTableFilterShowIncomingTxOnlyMsg = Just AddressDetails.TxTableFilterShowIncomingTxOnly
            , txTableFilterShowOutgoingTxOnlyMsg = Just AddressDetails.TxTableFilterShowOutgoingTxOnly
            , resetAllTxFiltersMsg = AddressDetails.ResetAllTxFilters
            , txTableAssetSelectBoxMsg = AddressDetails.TxTableAssetSelectBoxMsg
            , txTableFilterToggleZeroValueMsg = Nothing
            , openDateRangePickerMsg = Just AddressDetails.OpenDateRangePicker
            }
    in
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
        , case viewState.txs of
            RemoteData.Success txs ->
                if txs.isTxFilterViewOpen then
                    div
                        [ [ Css.position Css.fixed
                          , Css.right (Css.px 42)
                          , Css.top (Css.px 350)
                          , Css.property "transform" "translate(0%, -50%)"
                          , Css.zIndex (Css.int (Util.Css.zIndexMainValue + 1000))
                          ]
                            |> css
                        ]
                        [ TransactionFilter.txFilterDialogView vc (Id.network id) filterDialogMsgs txs |> Html.map (Pathfinder.AddressDetailsMsg viewState.address.id) ]

                else
                    none

            _ ->
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
            viewState.address.data
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\data -> Id.initClusterId data.currency data.entity)
                |> Maybe.andThen (flip Dict.get model.clusters)

        relatedAddressesTab =
            cluster
                |> Maybe.map
                    (relatedAddressesDataTab vc model id viewState
                        >> List.singleton
                    )
                |> Maybe.withDefault []

        relatedDataTabsList =
            transactionsOrNeighborsDataTabs vc model id viewState
                ++ relatedAddressesTab
                |> List.map (Html.map (Pathfinder.AddressDetailsMsg viewState.address.id))

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
            { clusterInfoVisible = cluster /= Nothing
            , clusterInfoInstance =
                cluster
                    |> Maybe.withDefault RemoteData.NotAsked
                    |> RemoteData.unpack (\_ -> loadingSpinner vc Css.View.loadingSpinner)
                        (clusterInfoView vc viewState.isClusterDetailsOpen model.colors
                            >> Html.map (Pathfinder.AddressDetailsMsg id)
                        )
            }

        assetId =
            assetFromBase <| Id.network viewState.address.id
    in
    SidePanelComponents.sidePanelAddressWithInstances
        (SidePanelComponents.sidePanelAddressAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelAddressDetails [ css fullWidth ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack (closeAttrs UserClosedDetailsView)
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
            |> Rs.s_sidePanelAddressDetails
                (viewState.address.data
                    |> RemoteData.map
                        (\_ -> Nothing)
                    |> RemoteData.withDefault (loadingSpinner vc Css.View.loadingSpinner |> Just)
                )
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
        , valueOfBalance = viewState.address.data |> RemoteData.map (.balance >> valuesToCell vc assetId) |> RemoteData.withDefault emptyCell
        , titleOfTotalReceived = { infoLabel = Locale.string vc.locale "Total received" }
        , valueOfTotalReceived = viewState.address.data |> RemoteData.map (.totalReceived >> valuesToCell vc assetId) |> RemoteData.withDefault emptyCell
        , titleOfTotalSent = { infoLabel = Locale.string vc.locale "Total sent" }
        , valueOfTotalSent = viewState.address.data |> RemoteData.map (.totalSpent >> valuesToCell vc assetId) |> RemoteData.withDefault emptyCell
        , titleOfLastUsage = { infoLabel = Locale.string vc.locale "Last usage" }
        , valueOfLastUsage = viewState.address.data |> RemoteData.map (.lastTx >> .timestamp >> timeToCell vc) |> RemoteData.withDefault emptyCell
        , titleOfFirstUsage = { infoLabel = Locale.string vc.locale "First usage" }
        , valueOfFirstUsage = viewState.address.data |> RemoteData.map (.firstTx >> .timestamp >> timeToCell vc) |> RemoteData.withDefault emptyCell

        -- , learnMoreButton = { variant = none }
        , categoryTags = { tagLabel = "", closeVisible = False }
        }


neighborsDataTab : View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Direction -> Html AddressDetails.Msg
neighborsDataTab vc model id viewState direction =
    let
        { lbl, getNoAddresses, getTableOpen, getTable } =
            case direction of
                Outgoing ->
                    { lbl = "Outgoing relations"
                    , getNoAddresses = .outDegree
                    , getTableOpen = .outgoingNeighborsTableOpen
                    , getTable = .neighborsOutgoing
                    }

                Incoming ->
                    { lbl = "Incoming relations"
                    , getNoAddresses = .inDegree
                    , getTableOpen = .incomingNeighborsTableOpen
                    , getTable = .neighborsIncoming
                    }

        label =
            Locale.string vc.locale lbl
                |> Locale.titleCase vc.locale
    in
    dataTab
        { title =
            SidePanelComponents.sidePanelListHeaderTitleWithNumberWithAttributes
                (SidePanelComponents.sidePanelListHeaderTitleWithNumberAttributes
                    |> Rs.s_root [ spread ]
                )
                { root =
                    { label = label
                    , number =
                        viewState.address.data
                            |> RemoteData.map (getNoAddresses >> Locale.int vc.locale)
                            |> RemoteData.withDefault ""
                    }
                }
        , disabled = RemoteData.map (getNoAddresses >> (<) 0) viewState.address.data /= RemoteData.Success True
        , content =
            case ( getTableOpen viewState, getTable viewState ) of
                ( True, RemoteData.Success tbl ) ->
                    let
                        conf =
                            { anchorId = id
                            , isChecked = flip Network.hasAggEdge model.network
                            , hasTags = getHavingTags model
                            , coinCode = assetFromBase <| Id.network viewState.address.id
                            , direction = direction
                            }
                    in
                    div
                        [ css <|
                            SidePanelComponents.sidePanelRelatedAddressesContent_details.styles
                                ++ fullWidth
                        ]
                        [ InfiniteTable.view vc
                            [ css fullWidth ]
                            (NeighborAddressesTable.config Css.Table.styles vc conf)
                            tbl
                        ]
                        |> Just

                _ ->
                    Nothing
        , onClick = AddressDetails.UserClickedToggleNeighborsTable direction
        }


getRelatedAddressTypeLabel : View.Config -> AddressDetails.RelatedAddressTypes -> String
getRelatedAddressTypeLabel vc relatedAddressType =
    case relatedAddressType of
        AddressDetails.Pubkey ->
            Locale.string vc.locale "Related by Public Key"

        AddressDetails.MultiInputCluster ->
            Locale.string vc.locale "Related by Multi-Input Heuristic"


relatedAddressesSelectBoxConfig : View.Config -> Id -> ThemedSelectBox.Config AddressDetails.RelatedAddressTypes b
relatedAddressesSelectBoxConfig vc id =
    { optionToLabel = getRelatedAddressTypeLabel vc
    , width = Nothing
    , filter =
        if isAccountLike (Id.network id) then
            (==) AddressDetails.Pubkey

        else
            always True
    }


relatedAddressesDataTab : View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> WebData Api.Data.Entity -> Html AddressDetails.Msg
relatedAddressesDataTab vc model _ viewState cluster =
    let
        label =
            Locale.string vc.locale
                "Cluster addresses"
                |> Locale.titleCase vc.locale

        noMultiInputAddresses =
            cluster
                |> RemoteData.map (.noAddresses >> flip (-) 1)
                |> RemoteData.withDefault 0

        pubkeyHasData =
            viewState.relatedAddressesPubkey
                |> RemoteData.map RelatedAddressesPubkeyTable.hasData
                |> RemoteData.withDefault False

        disabled =
            noMultiInputAddresses == 0 && not pubkeyHasData

        noRelatedAddresses =
            viewState.relatedAddressesVisibleTable
                |> Maybe.map
                    (\vt ->
                        case vt of
                            AddressDetails.MultiInputCluster ->
                                noMultiInputAddresses

                            AddressDetails.Pubkey ->
                                viewState.relatedAddressesPubkey
                                    |> RemoteData.map (RelatedAddressesPubkeyTable.getTable >> PagedTable.getNrItems)
                                    |> RemoteData.toMaybe
                                    |> Maybe.andThen identity
                                    |> Maybe.withDefault 0
                    )

        relatedAddressesVisibleTable =
            Maybe.withDefault AddressDetails.MultiInputCluster
                viewState.relatedAddressesVisibleTable
    in
    dataTab
        { title =
            SidePanelComponents.sidePanelListHeaderTitleWithNumberWithAttributes
                (SidePanelComponents.sidePanelListHeaderTitleWithNumberAttributes
                    |> Rs.s_root [ spread ]
                )
                { root =
                    { label = label
                    , number =
                        Maybe.map (Locale.int vc.locale) noRelatedAddresses
                            |> Maybe.withDefault ""
                    }
                }
        , disabled = disabled
        , content =
            if not viewState.relatedAddressesTableOpen || disabled then
                Nothing

            else
                Just
                    (div []
                        [ let
                            ttConfig =
                                { domId = "related_addresses_select_help"
                                , text =
                                    case relatedAddressesVisibleTable of
                                        AddressDetails.MultiInputCluster ->
                                            "Multi Input Cluster Help"
                                                |> Locale.string vc.locale

                                        AddressDetails.Pubkey ->
                                            "Pubkey Cluster Help"
                                                |> Locale.string vc.locale
                                }
                          in
                          div
                            [ css
                                [ Css.displayFlex
                                , Css.flexDirection Css.row
                                , Css.alignItems Css.center
                                , Css.margin (Css.px 5)
                                , Css.justifyContent Css.spaceBetween -- This pushes items to opposite ends
                                ]
                            ]
                            [ div
                                [ css [ Css.flexGrow (Css.int 1) ] ]
                                -- This makes the select box container take all available space
                                [ ThemedSelectBox.view (relatedAddressesSelectBoxConfig vc viewState.address.id)
                                    viewState.relatedAddressesVisibleTableSelectBox
                                    relatedAddressesVisibleTable
                                    |> Html.map AddressDetails.RelatedAddressesVisibleTableSelectBoxMsg
                                ]
                            , span
                                [ onMouseEnter (ShowRelatedAddressesTooltip ttConfig |> RelatedAddressesTooltipMsg |> AddressDetails.TooltipMsg)
                                , onMouseLeave (HideRelatedAddressesTooltip ttConfig |> RelatedAddressesTooltipMsg |> AddressDetails.TooltipMsg)
                                , Svg.Styled.Attributes.id ttConfig.domId
                                , css [ Css.flexShrink (Css.int 0) ] -- Prevent the icon from shrinking
                                ]
                                [ HIcons.iconsInfoSnoPaddingWithAttributes
                                    (HIcons.iconsInfoSnoPaddingAttributes
                                        |> Rs.s_shape [ fixFillRule ]
                                    )
                                    {}
                                ]
                            ]
                        , case relatedAddressesVisibleTable of
                            AddressDetails.MultiInputCluster ->
                                case viewState.relatedAddresses of
                                    RemoteData.Failure _ ->
                                        Html.text "error"

                                    RemoteData.Loading ->
                                        loadingSpinner vc Css.View.loadingSpinner

                                    RemoteData.NotAsked ->
                                        none

                                    RemoteData.Success ra ->
                                        let
                                            ratc =
                                                { isChecked = flip Network.hasAddress model.network
                                                , hasTags = getHavingTags model
                                                , coinCode = assetFromBase <| Id.network viewState.address.id
                                                }
                                        in
                                        div
                                            [ css <|
                                                SidePanelComponents.sidePanelRelatedAddressesContent_details.styles
                                                    ++ fullWidth
                                            ]
                                            [ InfiniteTable.view vc
                                                [ css fullWidth ]
                                                (RelatedAddressesTable.config Css.Table.styles vc ratc ra)
                                                (RelatedAddressesTable.getTable ra)
                                            ]

                            AddressDetails.Pubkey ->
                                case viewState.relatedAddressesPubkey of
                                    RemoteData.Failure _ ->
                                        Html.text "error"

                                    RemoteData.Loading ->
                                        loadingSpinner vc Css.View.loadingSpinner

                                    RemoteData.NotAsked ->
                                        none

                                    RemoteData.Success ra ->
                                        let
                                            ratc =
                                                { isChecked = flip Network.hasAddress model.network
                                                }
                                        in
                                        div
                                            [ css <|
                                                SidePanelComponents.sidePanelRelatedAddressesContent_details.styles
                                                    ++ fullWidth
                                            ]
                                            [ PagedTable.view vc
                                                [ css fullWidth ]
                                                (RelatedAddressesPubkeyTable.config Css.Table.styles vc ratc ra)
                                                (RelatedAddressesPubkeyTable.getTable ra)
                                                AddressDetails.RelatedAddressesPubkeyTablePagedTableMsg
                                            ]
                        ]
                    )
        , onClick = AddressDetails.UserClickedToggleRelatedAddressesTable
        }


clusterInfoView : View.Config -> Bool -> Colors.ScopedColorAssignment -> Api.Data.Entity -> Html AddressDetails.Msg
clusterInfoView vc open colors clstr =
    let
        text =
            Locale.string vc.locale "cluster-details-info-help-text"

        ctxtt =
            { text = text, domId = Sha256.sha256 text }

        ttAttributes =
            [ onMouseEnter (AddressDetails.ShowTextTooltip ctxtt |> RelatedAddressesTooltipMsg |> AddressDetails.TooltipMsg)
            , onMouseLeave (AddressDetails.HideTextTooltip ctxtt |> RelatedAddressesTooltipMsg |> AddressDetails.TooltipMsg)
            , Svg.Styled.Attributes.id ctxtt.domId
            ]

        helpIcon =
            Just <|
                HIcons.iconsInfoSnoPaddingWithAttributes
                    (HIcons.iconsInfoSnoPaddingAttributes
                        |> Rs.s_shape (fixFillRule :: ttAttributes)
                    )
                    {}
    in
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
            SidePanelComponents.clusterInformationOpenWithInstances
                (SidePanelComponents.clusterInformationOpenAttributes
                    |> Rs.s_root headerAttr
                    |> Rs.s_ellipse25 clusterColor
                )
                (SidePanelComponents.clusterInformationOpenInstances
                    |> Rs.s_iconsInfoSnoPadding helpIcon
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
            SidePanelComponents.clusterInformationClosedWithInstances
                (SidePanelComponents.clusterInformationClosedAttributes
                    |> Rs.s_root headerAttr
                )
                (SidePanelComponents.clusterInformationClosedInstances
                    |> Rs.s_iconsInfoSnoPadding helpIcon
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
                |> Inf.getPage
                |> List.map Tx.getTxIdForAddressTx
                |> allAndNotEmpty txOnGraphFn

        table =
            InfiniteTable.view vc
                []
                (TransactionTable.config styles vc addressId txOnGraphFn allChecked)
                model.table
    in
    [ TransactionFilter.filterHeader vc
        model
        { resetDateFilterMsg = AddressDetails.ResetDateRangePicker
        , resetAssetsFilterMsg = AddressDetails.ResetTxAssetFilter
        , resetDirectionFilterMsg = Just AddressDetails.ResetTxDirectionFilter
        , toggleFilterView = AddressDetails.ToggleTxFilterView
        , resetZeroValueFilterMsg = Nothing
        , exportCsv = Just ( AddressDetails.ExportCSVMsg, model.exportCSV )
        }
    , table
    ]
        |> div [ css [ Css.width (Css.pct 100) ] ]


transactionsDataTab : View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Html AddressDetails.Msg
transactionsDataTab vc model id viewState =
    let
        txOnGraphFn =
            flip Network.hasTx model.network

        noIncomingTxs =
            viewState.address.data
                |> RemoteData.map .noIncomingTxs
                |> RemoteData.withDefault 0

        noOutgoingTxs =
            viewState.address.data
                |> RemoteData.map .noOutgoingTxs
                |> RemoteData.withDefault 0

        totalNumber =
            noIncomingTxs + noOutgoingTxs
    in
    dataTab
        { title =
            SidePanelComponents.sidePanelListHeaderTitleTransactionsWithAttributes
                (SidePanelComponents.sidePanelListHeaderTitleTransactionsAttributes
                    |> Rs.s_root [ spread ]
                )
                { root =
                    { totalNumber =
                        totalNumber
                            |> Locale.int vc.locale
                    , incomingNumber =
                        viewState.address.data
                            |> RemoteData.map (.noIncomingTxs >> Locale.int vc.locale)
                            |> RemoteData.withDefault ""
                    , outgoingNumber =
                        viewState.address.data
                            |> RemoteData.map (.noOutgoingTxs >> Locale.int vc.locale)
                            |> RemoteData.withDefault ""
                    , title = Locale.string vc.locale "Transactions"
                    }
                }
        , disabled = totalNumber == 0
        , content =
            if viewState.transactionsTableOpen then
                viewState.txs
                    |> RemoteData.toMaybe
                    |> Maybe.map (transactionTableView vc id txOnGraphFn)

            else
                Nothing
        , onClick = AddressDetails.UserClickedToggleTransactionTable
        }


type alias AccountValueRundownConfig =
    { network : String
    , open : Bool
    , onClick : Pathfinder.Msg
    , values : Api.Data.Values
    , tokenValues : Maybe (Dict String Api.Data.Values)
    , title : String
    }


accountValueRundown : View.Config -> AccountValueRundownConfig -> Html Pathfinder.Msg
accountValueRundown vc conf =
    let
        fiatCurr =
            vc.preferredFiatCurrency

        getValue ( symbol, values ) =
            let
                ass =
                    asset conf.network symbol

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

        fiatSumTotalTokens =
            conf.tokenValues
                |> Maybe.withDefault Dict.empty
                |> Dict.toList
                |> List.filterMap (Tuple.second >> Locale.getFiatValue fiatCurr)
                |> List.sum

        fiatSumTotal =
            fiatSumTotalTokens
                + (conf.values
                    |> Locale.getFiatValue fiatCurr
                    |> Maybe.withDefault 0.0
                  )

        nativeValue =
            getValue ( conf.network, conf.values )

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
            [ onClick conf.onClick, fw, Util.View.pointer ]
    in
    if conf.open then
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
                    :: (conf.tokenValues
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
                , title = Locale.string vc.locale conf.title
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
                , title = Locale.string vc.locale conf.title
                , value = Locale.fiat vc.locale fiatCurr fiatSumTotal
                }
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
                    ++ (if RemoteData.map .isContract viewState.address.data == RemoteData.Success (Just True) then
                            Locale.string vc.locale "Smart Contract"

                        else
                            Locale.string vc.locale "Address"
                       )
            }

        totalReceivedRundown =
            viewState.address.data
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\data ->
                        accountValueRundown vc
                            { network = data.currency
                            , open = viewState.totalReceivedDetailsOpen
                            , onClick =
                                AddressDetails.UserClickedToggleTotalReceivedDetails
                                    |> Pathfinder.AddressDetailsMsg viewState.address.id
                            , title = "Total received"
                            , values = data.totalReceived
                            , tokenValues = data.totalTokensReceived
                            }
                    )

        totalSentRundown =
            viewState.address.data
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\data ->
                        accountValueRundown vc
                            { network = data.currency
                            , open = viewState.totalSentDetailsOpen
                            , onClick =
                                AddressDetails.UserClickedToggleTotalSpentDetails
                                    |> Pathfinder.AddressDetailsMsg viewState.address.id
                            , title = "Total sent"
                            , values = data.totalSpent
                            , tokenValues = data.totalTokensSpent
                            }
                    )

        balanceRundown =
            viewState.address.data
                |> RemoteData.toMaybe
                |> Maybe.map
                    (\data ->
                        accountValueRundown vc
                            { network = data.currency
                            , open = viewState.balanceDetailsOpen
                            , onClick =
                                AddressDetails.UserClickedToggleBalanceDetails
                                    |> Pathfinder.AddressDetailsMsg viewState.address.id
                            , title = "Balance"
                            , values = data.balance
                            , tokenValues = data.tokenBalances
                            }
                    )

        relatedAddressesTab =
            [ relatedAddressesDataTab vc model id viewState RemoteData.NotAsked ]

        relatedDataTabsList =
            transactionsOrNeighborsDataTabs vc model id viewState
                ++ relatedAddressesTab
                |> List.map (Html.map (Pathfinder.AddressDetailsMsg viewState.address.id))
    in
    SidePanelComponents.sidePanelEthAddressWithInstances
        (SidePanelComponents.sidePanelEthAddressAttributes
            |> Rs.s_root
                [ sidePanelCss
                    |> css
                ]
            |> Rs.s_sidePanelHeaderText [ spread ]
            |> Rs.s_iconsCloseBlack (closeAttrs UserClosedDetailsView)
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
            |> Rs.s_totalReceivedRow totalReceivedRundown
            |> Rs.s_totalSentRow totalSentRundown
            |> Rs.s_balanceRow balanceRundown
            |> Rs.s_sidePanelEthAddressDetails
                (viewState.address.data
                    |> RemoteData.map
                        (\_ -> Nothing)
                    |> RemoteData.withDefault (loadingSpinner vc Css.View.loadingSpinner |> Just)
                )
        )
        { pluginList = pluginList
        , pluginTagsList = pluginTagsList
        , relatedDataTabsList = relatedDataTabsList
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
        , valueOfLastUsage = viewState.address.data |> RemoteData.map (.lastTx >> .timestamp >> timeToCell vc) |> RemoteData.withDefault emptyCell
        , titleOfFirstUsage = { infoLabel = Locale.string vc.locale "First usage" }
        , valueOfFirstUsage = viewState.address.data |> RemoteData.map (.firstTx >> .timestamp >> timeToCell vc) |> RemoteData.withDefault emptyCell
        , categoryTags = { tagLabel = "", closeVisible = False }
        , balanceRow = { iconInstance = none, title = "", value = "" }
        , totalSentRow = { iconInstance = none, title = "", value = "" }
        , sidePanelRowChevronOpen = { iconInstance = none, title = "", value = "" }
        }


transactionsOrNeighborsDataTabs : View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> List (Html AddressDetails.Msg)
transactionsOrNeighborsDataTabs vc model id viewState =
    case model.config.tracingMode of
        TransactionTracingMode ->
            [ transactionsDataTab vc model id viewState
            ]

        AggregateTracingMode ->
            [ neighborsDataTab vc model id viewState Outgoing
            , neighborsDataTab vc model id viewState Incoming
            ]


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
                        >> Html.map (TagTooltipMsg >> AddressDetails.TooltipMsg)
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
    , copyIconInstance = Id.id id |> copyIconPathfinderAbove vc
    , addTagIconInstance =
        iconWithHint
            vc
            { hint = Locale.string vc.locale "Report tag"
            , icon =
                HIcons.iconsAddTagOutlinedSWithAttributes
                    (HIcons.iconsAddTagOutlinedSAttributes
                        |> Rs.s_root [ onClick (Pathfinder.UserOpensDialogWindow (Pathfinder.AddTags id)), Util.View.pointer ]
                    )
                    {}
            , hide = False
            , position = Above
            }
            []
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
