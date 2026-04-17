module View.Pathfinder.AddressDetails exposing (view)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as Inf
import Components.PagedTable as PagedTable
import Components.Tooltip as Tooltip
import Components.TransactionFilter as TransactionFilter
import Config.Pathfinder exposing (TracingMode(..))
import Config.View as View
import Css
import Css.Pathfinder exposing (fullWidth, sidePanelCss)
import Css.Table
import Css.View
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html, div, img)
import Html.Styled.Attributes as HA exposing (src)
import Html.Styled.Events exposing (onClick, onMouseEnter, onMouseLeave, preventDefaultOn, stopPropagationOn)
import Init.Pathfinder.Id as Id
import Json.Decode
import Model.Currency exposing (asset, assetFromBase)
import Model.Direction exposing (Direction(..))
import Model.Graph.Coords as Coords
import Model.Locale as Locale
import Model.Pathfinder as Pathfinder exposing (getHavingTags, getSortedConceptsByWeight, getSortedLabelSummariesByRelevance, getTagSummary)
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
import Msg.Pathfinder as Pathfinder exposing (OverlayWindows(..))
import Msg.Pathfinder.AddressDetails as AddressDetails exposing (Msg(..))
import Plugin.Model exposing (ModelState)
import Plugin.View as Plugin exposing (Plugins)
import RecordSetter as Rs
import RemoteData exposing (WebData)
import Set
import Svg.Styled exposing (Svg)
import Svg.Styled.Attributes exposing (css)
import Theme.Html.Icons as HIcons
import Theme.Html.SidePanelComponents as SidePanelComponents
import Theme.Html.TagsComponents as TagsComponents
import Util exposing (allAndNotEmpty)
import Util.Css exposing (spread)
import Util.Data as Data exposing (isAccountLike)
import Util.ExternalLinks exposing (addProtocolPrefx)
import Util.Graph exposing (decodeCoords)
import Util.Pathfinder as Pathfinder
import Util.Pathfinder.TagSummary exposing (hasOnlyExchangeTags)
import Util.Tag as Tag
import Util.ThemedSelectBox as ThemedSelectBox
import Util.Tooltip
import Util.TooltipType
import Util.View exposing (HintPosition(..), copyIconPathfinderAbove, emptyCell, iconWithHint, loadingSpinner, none, timeToCell, truncateLongIdentifierWithLengths)
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
        ]


categoriesMaxWidth : Float
categoriesMaxWidth =
    300


utxo : Plugins -> ModelState -> View.Config -> Pathfinder.Model -> Id -> AddressDetails.Model -> Address -> Html Pathfinder.Msg
utxo plugins pluginStates vc model id viewState address =
    let
        crosschainTargets =
            crosschainLedgerTargets id address

        crosschainVisible =
            not (List.isEmpty crosschainTargets)

        crosschainLedgersList =
            crosschainTargets
                |> List.map
                    (\( network, targetId ) ->
                        div
                            [ onClick (Pathfinder.UserClickedAddress targetId)
                            , css [ Css.cursor Css.pointer ]
                            ]
                            [ TagsComponents.categoryTag
                                { root =
                                    { tagLabel = network
                                    , closeVisible = False
                                    }
                                }
                            ]
                    )

        pluginTagsVisible =
            List.length pluginTagsList > 0

        { sidePanelData, categoriesList, hasClusterOnlyTags } =
            makeSidePanelData vc model id pluginTagsVisible crosschainVisible

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
                Address.toNodeIconHtml address
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
                        (clusterInfoView vc viewState.isClusterDetailsOpen model.colors viewState
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
            |> Rs.s_iconsCloseBlack (closeAttrs Pathfinder.UserClosedDetailsView)
            |> Rs.s_pluginList [ css [ Css.display Css.none ] ]
            |> Rs.s_categoriesList [ css [ Css.maxWidth <| Css.px categoriesMaxWidth ] ]
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
            |> Rs.s_crosschainLedgers
                (if crosschainVisible then
                    []

                 else
                    [ css [ Css.display Css.none ] ]
                )
        )
        (SidePanelComponents.sidePanelAddressInstances
            |> Rs.s_labelOfActor (labelOfActor vc model id)
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
        , ledgersList = crosschainLedgersList
        , categoriesList = categoriesList
        }
        { root = sidePanelData
        , iconsTagL =
            { variant =
                if List.isEmpty categoriesList then
                    none

                else if hasClusterOnlyTags then
                    HIcons.iconsTagLTypeIndirect {}

                else
                    HIcons.iconsTagLTypeDirect {}
            }
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
            Locale.string vc.locale "Related by public key"

        AddressDetails.MultiInputCluster ->
            Locale.string vc.locale "Related by multi-input heuristic"


relatedAddressesSelectBoxConfig : View.Config -> Id -> ThemedSelectBox.Config AddressDetails.RelatedAddressTypes
relatedAddressesSelectBoxConfig vc id =
    ThemedSelectBox.defaultConfig
        (getRelatedAddressTypeLabel vc)
        |> ThemedSelectBox.withFilter
            (if isAccountLike (Id.network id) then
                (==) AddressDetails.Pubkey

             else
                always True
            )


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
                            helpText =
                                case relatedAddressesVisibleTable of
                                    AddressDetails.MultiInputCluster ->
                                        "Multi Input Cluster Help"

                                    AddressDetails.Pubkey ->
                                        "Pubkey Cluster Help"

                            tooltipConfig =
                                Util.Tooltip.tooltipConfig vc AddressDetails.TooltipMsg
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
                            , HIcons.iconsInfoSnoPaddingDevWithAttributes
                                (HIcons.iconsInfoSnoPaddingDevAttributes
                                    |> Rs.s_root
                                        (Util.TooltipType.Text helpText |> Tooltip.attributes "related-addresses-tooltip" tooltipConfig)
                                )
                                {}
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


clusterInfoView : View.Config -> Bool -> Colors.ScopedColorAssignment -> AddressDetails.Model -> Api.Data.Entity -> Html AddressDetails.Msg
clusterInfoView vc open colors viewState clstr =
    let
        tooltipConfig =
            Util.Tooltip.tooltipConfig vc AddressDetails.TooltipMsg

        helpIcon =
            Just <|
                HIcons.iconsInfoSnoPaddingDevWithAttributes
                    (HIcons.iconsInfoSnoPaddingDevAttributes
                        |> Rs.s_root
                            (Util.TooltipType.Text "cluster-details-info-help-text"
                                |> Tooltip.attributes "address-details-text-tooltip" tooltipConfig
                            )
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
                Locale.string vc.locale "Cluster-info"

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
                , titleOfNumberOfAddresses = { infoLabel = Locale.string vc.locale "Number-of-addresses" }
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


transactionTableView : View.Config -> Id -> (Id -> Bool) -> Pathfinder.Model -> TransactionTable.Model -> Html AddressDetails.Msg
transactionTableView vc addressId txOnGraphFn model txs =
    let
        styles =
            Css.Table.styles

        allChecked =
            txs.table
                |> Inf.getPage
                |> List.map Tx.getTxIdForAddressTx
                |> allAndNotEmpty txOnGraphFn

        table =
            InfiniteTable.view vc
                []
                (TransactionTable.config styles vc addressId txOnGraphFn allChecked)
                txs.table
    in
    [ TransactionFilter.view vc
        (Id.network addressId)
        { tag = TransactionFilterMsg
        , exportCsv = Just ( AddressDetails.ExportCSVMsg txs, model.exportCSV )
        , tooltipConfig = Util.Tooltip.tooltipConfig vc AddressDetails.TooltipMsg
        }
        txs.filter
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
                    |> Maybe.map (transactionTableView vc id txOnGraphFn model)

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
        crosschainTargets =
            crosschainLedgerTargets id address

        crosschainVisible =
            not (List.isEmpty crosschainTargets)

        crosschainLedgersList =
            crosschainTargets
                |> List.map
                    (\( network, targetId ) ->
                        div
                            [ onClick (Pathfinder.UserClickedAddress targetId)
                            , css [ Css.cursor Css.pointer ]
                            ]
                            [ TagsComponents.categoryTag
                                { root =
                                    { tagLabel = network
                                    , closeVisible = False
                                    }
                                }
                            ]
                    )

        pluginList =
            Plugin.addressSidePanelHeader plugins pluginStates vc address

        pluginTagsList =
            Plugin.addressSidePanelHeaderTags plugins pluginStates vc address

        pluginTagsVisible =
            List.length pluginTagsList > 0

        { sidePanelData, categoriesList, hasClusterOnlyTags } =
            makeSidePanelData vc model id pluginTagsVisible crosschainVisible

        sidePanelAddressHeader =
            { iconInstance =
                Address.toNodeIconHtml address
            , headerText =
                (String.toUpper <| Id.network id)
                    ++ " "
                    ++ (if RemoteData.map .isContract viewState.address.data == RemoteData.Success (Just True) then
                            Locale.string vc.locale "Smart contract"

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
            |> Rs.s_iconsCloseBlack (closeAttrs Pathfinder.UserClosedDetailsView)
            |> Rs.s_pluginList [ css [ Css.display Css.none ] ]
            |> Rs.s_categoriesList [ css [ Css.maxWidth <| Css.px categoriesMaxWidth ] ]
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
            |> Rs.s_crosschainLedgers
                (if crosschainVisible then
                    []

                 else
                    [ css [ Css.display Css.none ] ]
                )
        )
        (SidePanelComponents.sidePanelEthAddressInstances
            |> Rs.s_labelOfActor (labelOfActor vc model id)
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
        , ledgersList = crosschainLedgersList
        , categoriesList = categoriesList
        }
        { identifierWithCopyIcon = sidePanelAddressCopyIcon vc id
        , iconsTagL =
            { variant =
                if List.isEmpty categoriesList then
                    none

                else if hasClusterOnlyTags then
                    HIcons.iconsTagLTypeIndirect {}

                else
                    HIcons.iconsTagLTypeDirect {}
            }
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


tagsList : View.Config -> Pathfinder.Model -> Id -> List (Html Pathfinder.Msg)
tagsList vc model id =
    let
        clusterOnlyTagsVisible =
            isClusterOnlyTags model id

        ts =
            getTagSummary model id

        nMaxTags =
            2

        tagsTruncated renderItem items =
            (items |> List.take nMaxTags |> List.map renderItem)
                ++ (if List.length items > nMaxTags then
                        [ TagsComponents.moreItemsInfo
                            { root = { number = String.fromInt (List.length items - nMaxTags) } }
                        ]

                    else
                        []
                   )
    in
    if clusterOnlyTagsVisible then
        [ learnMoreButton vc id ]

    else if vc.showLabelsInTaggingOverview then
        let
            showTag ( tid, t ) =
                Html.div
                    ([ HA.css SidePanelComponents.sidePanelAddressSidePanelHeaderTags_details.styles
                    , css [ Css.cursor Css.pointer ]
                    , onClick (Pathfinder.UserOpensDialogWindow (TagsList id))
                    ]
                        ++ (Util.TooltipType.TagLabel id tid
                                |> Tooltip.attributes tid (Util.Tooltip.tooltipConfig vc Pathfinder.TooltipMsg)
                           )
                    )
                    [ Html.text t.label
                    ]

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
        in
        tagLabels
            |> tagsTruncated showTag

    else
        let
            concepts =
                ts
                    |> Maybe.map getSortedConceptsByWeight
                    |> Maybe.withDefault []
        in
        (concepts
            |> tagsTruncated
                (Tag.conceptItem vc id AddressDetails.TooltipMsg
                    >> Html.map (Pathfinder.AddressDetailsMsg id)
                )
        )
            ++ [ learnMoreButton vc id ]


learnMoreButton : View.Config -> Id -> Html Pathfinder.Msg
learnMoreButton vc id =
    Button.defaultConfig
        |> Rs.s_text "learn more"
        |> Rs.s_onClick (Just (Pathfinder.UserOpensDialogWindow (TagsList id)))
        |> Button.linkButtonBlue vc


crosschainLedgerTargets : Id -> Address -> List ( String, Id )
crosschainLedgerTargets id address =
    address.networks
        |> Dict.toList
        |> List.filter (\( network, addresses ) -> network /= Id.network id && not (Set.isEmpty addresses))
        |> List.filterMap
            (\( network, addresses ) ->
                addresses
                    |> Set.toList
                    |> List.minimum
                    |> Maybe.map
                        (\targetAddress ->
                            ( String.toUpper network
                            , Id.init network targetAddress
                            )
                        )
            )
        |> List.sortBy Tuple.first


makeSidePanelData : View.Config -> Pathfinder.Model -> Id -> Bool -> Bool -> { sidePanelData : { actorIconInstance : Svg msg, tabsVisible : Bool, tagSectionVisible : Bool, pluginSTagVisible : Bool, actorVisible : Bool, tagsVisible : Bool }, categoriesList : List (Html Pathfinder.Msg), hasClusterOnlyTags : Bool }
makeSidePanelData vc model id pluginTagsVisible crosschainVisible =
    let
        clusterOnlyTagsVisible =
            isClusterOnlyTags model id

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

        categoriesList =
            tagsList vc model id

        showOtherTag =
            nrTagsAddress > 0 || clusterOnlyTagsVisible
    in
    { sidePanelData =
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
        , tagSectionVisible = showExchangeTag || showOtherTag || pluginTagsVisible || crosschainVisible
        , pluginSTagVisible = pluginTagsVisible
        , actorVisible = showExchangeTag
        , tagsVisible = showOtherTag || crosschainVisible
        }
    , categoriesList = categoriesList
    , hasClusterOnlyTags = clusterOnlyTagsVisible
    }


isClusterOnlyTags : Pathfinder.Model -> Id -> Bool
isClusterOnlyTags model id =
    case getHavingTags model id of
        Pathfinder.HasTagSummaryOnlyWithCluster _ ->
            True

        Pathfinder.HasClusterTagsOnlyButNoDirect ->
            True

        _ ->
            False


labelOfActor : View.Config -> Pathfinder.Model -> Id -> Maybe (Html Pathfinder.Msg)
labelOfActor vc model id =
    let
        ts =
            getTagSummary model id

        actor_id =
            ts
                |> Maybe.andThen .bestActor

        actor =
            actor_id
                |> Maybe.andThen (\i -> Dict.get i model.actors)

        actorText =
            actor
                |> Maybe.map .label
    in
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
