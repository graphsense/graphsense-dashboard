module Util.Pathfinder.History exposing (shallPushHistory)

import Model.Pathfinder exposing (Model)
import Msg.Pathfinder exposing (Msg(..))
import Msg.Pathfinder.AddressDetails as AddressDetails
import Plugin.Update as Plugin exposing (Plugins)


shallPushHistory : Plugins -> Msg -> Model -> Bool
shallPushHistory plugins msg _ =
    case msg of
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

        AddressDetailsMsg _ am ->
            case am of
                AddressDetails.UserClickedTxCheckboxInTable _ ->
                    True

                AddressDetails.UserClickedToggleNeighborsTable ->
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

                AddressDetails.TransactionsTablePagedTableMsg _ ->
                    False

                AddressDetails.NeighborsTablePagedTableMsg _ _ ->
                    False

                AddressDetails.GotTxsForAddressDetails _ _ ->
                    False

                AddressDetails.GotNextPageTxsForAddressDetails _ ->
                    False

                AddressDetails.GotNeighborsForAddressDetails _ _ ->
                    False

                AddressDetails.UpdateDateRangePicker _ ->
                    False

                AddressDetails.OpenDateRangePicker ->
                    False

                AddressDetails.CloseDateRangePicker ->
                    False

                AddressDetails.ResetDateRangePicker ->
                    False

                AddressDetails.BrowserGotFromDateBlock _ _ ->
                    False

                AddressDetails.BrowserGotToDateBlock _ _ ->
                    False

                AddressDetails.TableMsg _ ->
                    False

                AddressDetails.RelatedAddressesTableMsg _ ->
                    False

                AddressDetails.BrowserGotEntityAddressesForRelatedAddressesTable _ ->
                    False

                AddressDetails.BrowserGotEntityAddressTagsForRelatedAddressesTable _ _ ->
                    False

                AddressDetails.UserClickedToggleRelatedAddressesTable ->
                    False

                AddressDetails.RelatedAddressesTablePagedTableMsg _ ->
                    False

                AddressDetails.UserClickedAddressCheckboxInTable _ ->
                    True

                AddressDetails.NoOp ->
                    False

                AddressDetails.UserClickedTx _ ->
                    False

                AddressDetails.BrowserGotAddressesForTags _ _ ->
                    False

                AddressDetails.TooltipMsg _ ->
                    False

        TxDetailsMsg _ ->
            False

        AnimationFrameDeltaForTransform _ ->
            False

        AnimationFrameDeltaForMove _ ->
            False

        BrowserGotAddressData _ _ _ ->
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

        BrowserGotTxForAddress _ _ _ ->
            False

        BrowserGotActor _ _ ->
            False

        BrowserGotTx _ _ _ ->
            False

        ChangedDisplaySettingsMsg _ ->
            False

        UserClickedTx _ ->
            False

        UserClickedAddressCheckboxInTable _ ->
            True

        WorkflowNextUtxoTx _ _ ->
            False

        WorkflowNextTxByTime _ _ ->
            False

        UserPushesLeftMouseButtonOnUtxoTx _ _ ->
            False

        UserClickedRemoveAddressFromGraph _ ->
            True

        UserReleasedDeleteKey ->
            True

        UserMovesMouseOverUtxoTx _ ->
            False

        UserMovesMouseOutUtxoTx _ ->
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
            False

        UserSelectsAnnotationColor _ _ ->
            False

        ToolbarHovercardMsg _ ->
            False

        UserClickedExportGraphAsImage _ ->
            False

        UserClickedToggleClusterDetailsOpen ->
            False

        UserClickedToggleDisplayAllTagsInDetails ->
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
