module Util.Pathfinder.History exposing (shallPushHistory)

import Model.Pathfinder exposing (Model)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails as AddressDetails
import Msg.Pathfinder.RelationDetails as RelationDetails
import Plugin.Update as Plugin exposing (Plugins)


shallPushHistory : Plugins -> Msg -> Model -> Bool
shallPushHistory plugins msg _ =
    case msg of
        InternalConversionLoopAddressesLoaded _ ->
            False

        EventualMessagesHeartBeat ->
            False

        UserClickedGraph _ ->
            False

        SearchMsg _ ->
            False

        UserPushesLeftMouseButtonOnGraph _ ->
            False

        UserMovesMouseOnGraph _ ->
            False

        UserWheeledOnGraph _ _ _ ->
            False

        PluginMsg pmsg ->
            Plugin.shallPushHistory plugins pmsg

        UserPushesLeftMouseButtonOnAddress _ _ ->
            False

        UserReleasesMouseButton ->
            False

        UserToggleAnnotationSettings ->
            False

        UserOpensAddressAnnotationDialog _ ->
            False

        UserOpensTxAnnotationDialog _ ->
            False

        UserClickedRestart ->
            False

        UserClickedRestartYes ->
            True

        UserClickedUndo ->
            False

        UserClickedRedo ->
            False

        UserClosedDetailsView ->
            False

        UserPressedModKey ->
            False

        UserReleasedModKey ->
            False

        UserReleasedEscape ->
            False

        UserPressedNormalKey _ ->
            False

        UserReleasedNormalKey _ ->
            False

        UserClickedShowLegend ->
            False

        UserClickedToggleHelpDropdown ->
            False

        AddressDetailsMsg _ am ->
            case am of
                AddressDetails.RelatedAddressesVisibleTableSelectBoxMsg _ ->
                    False

                AddressDetails.RelatedAddressesPubkeyTablePagedTableMsg _ ->
                    False

                AddressDetails.BrowserGotPubkeyRelations _ ->
                    False

                AddressDetails.UserClickedTxCheckboxInTable _ ->
                    True

                AddressDetails.UserClickedAllTxCheckboxInTable ->
                    True

                AddressDetails.UserClickedToggleNeighborsTable _ ->
                    False

                AddressDetails.UserClickedToggleTokenBalancesSelect ->
                    False

                AddressDetails.UserClickedToggleTransactionTable ->
                    False

                AddressDetails.UserClickedToggleBalanceDetails ->
                    False

                AddressDetails.UserClickedToggleTotalReceivedDetails ->
                    False

                AddressDetails.UserClickedToggleTotalSpentDetails ->
                    False

                AddressDetails.UserClickedToggleClusterDetailsOpen ->
                    False

                AddressDetails.UserClickedToggleDisplayAllTagsInDetails ->
                    False

                AddressDetails.TransactionsTableSubTableMsg _ ->
                    False

                AddressDetails.NeighborsTableSubTableMsg _ _ ->
                    False

                AddressDetails.GotTxsForAddressDetails _ _ ->
                    False

                AddressDetails.ToggleTxFilterView ->
                    False

                AddressDetails.CloseTxFilterView ->
                    False

                AddressDetails.GotNeighborsForAddressDetails _ _ _ ->
                    False

                AddressDetails.UpdateDateRangePicker _ ->
                    False

                AddressDetails.OpenDateRangePicker ->
                    False

                AddressDetails.CloseDateRangePicker ->
                    False

                AddressDetails.TxTableFilterShowAllTxs ->
                    False

                AddressDetails.TxTableFilterShowIncomingTxOnly ->
                    False

                AddressDetails.TxTableFilterShowOutgoingTxOnly ->
                    False

                AddressDetails.ResetDateRangePicker ->
                    False

                AddressDetails.ResetAllTxFilters ->
                    False

                AddressDetails.ResetTxAssetFilter ->
                    False

                AddressDetails.ResetTxDirectionFilter ->
                    False

                AddressDetails.TableMsg _ ->
                    False

                AddressDetails.RelatedAddressesTableMsg _ ->
                    False

                AddressDetails.RelatedAddressesPubkeyTableMsg _ ->
                    False

                AddressDetails.BrowserGotEntityAddressesForRelatedAddressesTable _ ->
                    False

                AddressDetails.BrowserGotEntityAddressTagsForRelatedAddressesTable _ _ ->
                    False

                AddressDetails.UserClickedToggleRelatedAddressesTable ->
                    False

                AddressDetails.RelatedAddressesTableSubTableMsg _ ->
                    False

                AddressDetails.UserClickedAddressCheckboxInTable _ ->
                    True

                AddressDetails.UserClickedAggEdgeCheckboxInTable _ _ _ ->
                    True

                AddressDetails.NoOp ->
                    False

                AddressDetails.UserClickedTx _ ->
                    False

                AddressDetails.BrowserGotAddressesForTags _ _ ->
                    False

                AddressDetails.TooltipMsg _ ->
                    False

                AddressDetails.TxTableAssetSelectBoxMsg _ ->
                    False

                AddressDetails.ExportCSVMsg _ _ ->
                    False

                AddressDetails.GotAddressTxsForExport _ _ ->
                    False

                AddressDetails.BrowserGotBulkTxsForExport _ _ _ _ _ _ ->
                    False

                AddressDetails.BrowserGotBulkTagsForExport _ _ _ _ ->
                    False

        TxDetailsMsg _ ->
            False

        RelationDetailsMsg _ ms ->
            case ms of
                RelationDetails.UserClickedAllTxCheckboxInTable _ ->
                    True

                RelationDetails.UserClickedTxCheckboxInTable _ ->
                    True

                RelationDetails.UserClickedToggleTable _ ->
                    False

                RelationDetails.TableMsg _ _ ->
                    False

                RelationDetails.BrowserGotLinks _ _ _ ->
                    False

                RelationDetails.BrowserGotLinksForExport _ _ _ ->
                    False

                RelationDetails.UserClickedTx _ ->
                    False

                RelationDetails.NoOp ->
                    False

                RelationDetails.ToggleTxFilterView _ ->
                    False

                RelationDetails.CloseTxFilterView _ ->
                    False

                RelationDetails.OpenDateRangePicker _ ->
                    False

                RelationDetails.CloseDateRangePicker _ ->
                    False

                RelationDetails.ResetDateRangePicker _ ->
                    False

                RelationDetails.ResetAllTxFilters _ ->
                    False

                RelationDetails.ResetTxAssetFilter _ ->
                    False

                RelationDetails.TxTableAssetSelectBoxMsg _ _ ->
                    False

                RelationDetails.UpdateDateRangePicker _ _ ->
                    False

                RelationDetails.ExportCSVMsg _ _ _ ->
                    False

        AnimationFrameDeltaForTransform _ ->
            False

        AnimationFrameDeltaForMove _ ->
            False

        BrowserGotAddressData _ _ ->
            False

        BrowserGotClusterData _ _ ->
            False

        BrowserGotAddressesTags _ _ ->
            False

        BrowserGotTagSummary _ _ _ ->
            False

        UserClickedAddressExpandHandle _ _ ->
            True

        UserClickedAddress _ ->
            False

        NoOp ->
            False

        BrowserGotActor _ _ ->
            False

        BrowserGotTx _ _ ->
            False

        BrowserGotTxFlow _ _ _ ->
            False

        BrowserGotConversionLoop _ _ _ ->
            False

        BrowserGotConversions _ _ ->
            False

        ChangedDisplaySettingsMsg _ ->
            False

        UserClickedTx _ ->
            False

        UserClickedAddressCheckboxInTable _ ->
            True

        UserClickedAllAddressCheckboxInTable _ ->
            True

        WorkflowNextUtxoTx _ _ _ ->
            False

        WorkflowNextTxByTime _ _ _ ->
            False

        UserPushesLeftMouseButtonOnUtxoTx _ _ ->
            False

        UserClickedRemoveAddressFromGraph _ ->
            True

        UserReleasedDeleteKey ->
            True

        UserMovesMouseOverTx _ ->
            False

        UserMovesMouseOutTx _ ->
            False

        UserMovesMouseOverAggEdge _ ->
            False

        UserMovesMouseOutConversionEdge _ _ ->
            False

        UserMovesMouseOverConversionEdge _ _ ->
            False

        UserMovesMouseOutAggEdge _ ->
            False

        UserMovesMouseOverAddress _ ->
            False

        UserMovesMouseOutAddress _ ->
            False

        UserMovesMouseOverTagLabel _ ->
            False

        UserMovesMouseOutTagLabel _ ->
            False

        UserMovesMouseOverActorLabel _ ->
            False

        UserMovesMouseOutActorLabel _ ->
            False

        UserInputsAnnotation _ _ ->
            True

        UserSelectsAnnotationColor _ _ ->
            True

        ToolbarHovercardMsg _ ->
            False

        UserClickedExportGraphAsImage _ ->
            False

        UserClickedExportGraphAsPdf _ ->
            False

        BrowserSentBBox _ ->
            False

        BrowserSentExportGraphResult _ ->
            False

        UserClickedExportGraphTxsAsCSV _ ->
            False

        BrowserGotTagSummariesForExportGraphTxsAsCSV _ _ _ ->
            False

        UserClickedToolbarDeleteIcon ->
            True

        UserClickedContextMenuDeleteIcon _ ->
            True

        UserClickedFitGraph ->
            False

        UserClickedSelectionTool ->
            False

        UserClickedSaveGraph _ ->
            False

        UserClickedOpenGraph ->
            False

        BrowserGotBulkAddresses _ ->
            False

        BrowserGotBulkTxs _ _ ->
            False

        UserOpensContextMenu _ _ ->
            False

        UserClosesContextMenu ->
            False

        UserClickedContextMenuOpenInNewTab _ ->
            False

        UserClickedContextMenuIdToClipboard _ ->
            False

        RuntimePostponedUpdateByRoute _ ->
            False

        BrowserWaitedAfterReleasingMouseButton ->
            False

        UserOpensDialogWindow _ ->
            False

        UserGotDataForTagsListDialog _ _ ->
            False

        ShowTextTooltip _ ->
            False

        CloseTextTooltip _ ->
            False

        BrowserGotTagSummaries _ _ ->
            False

        UserClickedToggleTracingMode ->
            False

        BrowserGotRelationsToVisibleNeighbors _ _ ->
            False

        InternalPathfinderAddedAddress _ ->
            False

        UserClickedAggEdge _ ->
            False

        UserClickedConversionEdge _ _ ->
            False

        ConversionDetailsMsg _ _ ->
            False

        UserClickedContextMenuAlignVertically ->
            True

        UserClickedContextMenuAlignHorizontally ->
            True
