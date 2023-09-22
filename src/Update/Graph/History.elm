module Update.Graph.History exposing (shallPushHistory)

import Msg.Graph exposing (Msg(..))


shallPushHistory : Msg -> Bool
shallPushHistory msg =
    case msg of
        UserClickedEntityExpandHandle _ _ ->
            True

        UserClickedAddressExpandHandle _ _ ->
            True

        UserClickedAddressesExpand _ ->
            True

        UserClickedRemoveAddress _ ->
            True

        UserClickedRemoveEntity _ ->
            True

        UserClickedAddressInEntityAddressesTable _ _ ->
            True

        UserClickedAddressInNeighborsTable _ _ _ ->
            True

        UserClickedEntityInNeighborsTable _ _ _ ->
            True

        UserSubmitsTagInput ->
            True

        UserSubmitsSearchInput ->
            True

        UserClickedNewYes ->
            True

        UserPressesDelete ->
            True

        UserClickedRemoveAddressLink _ ->
            True

        UserClickedRemoveEntityLink _ ->
            True

        UserClickedGraph _ ->
            False

        UserClickedAddress _ ->
            False

        UserRightClickedAddress _ _ ->
            False

        UserClickedAddressActions _ _ ->
            False

        UserHoversAddress _ ->
            False

        UserClickedEntity _ _ ->
            False

        UserRightClickedEntity _ _ ->
            False

        UserClickedEntityActions _ _ ->
            False

        UserHoversEntity _ ->
            False

        UserHoversEntityLink _ ->
            False

        UserClicksEntityLink _ ->
            False

        UserRightClicksEntityLink _ _ ->
            False

        UserHoversAddressLink _ ->
            False

        UserClicksAddressLink _ ->
            False

        UserRightClicksAddressLink _ _ ->
            False

        UserClickedTransactionActions _ _ _ ->
            False

        UserLeavesThing ->
            False

        UserPushesLeftMouseButtonOnGraph _ ->
            False

        UserMovesMouseOnGraph _ ->
            False

        UserReleasesMouseButton ->
            False

        BrowserGotBrowserElement _ ->
            False

        UserWheeledOnGraph _ _ _ ->
            False

        UserPushesLeftMouseButtonOnEntity _ _ ->
            False

        BrowserGotEntityNeighbors _ _ _ ->
            False

        BrowserGotEntityEgonet _ _ _ _ ->
            False

        BrowserGotEntityEgonetForAddress _ _ _ _ _ ->
            False

        BrowserGotAddressEgonet _ _ _ ->
            False

        BrowserGotAddressNeighbors _ _ _ ->
            False

        BrowserGotAddressNeighborsTable _ _ _ ->
            False

        BrowserGotNow _ ->
            False

        BrowserGotAddress _ ->
            False

        BrowserGotActor _ ->
            False

        BrowserGotEntity _ ->
            False

        BrowserGotBlock _ ->
            False

        BrowserGotEntityForAddress _ _ ->
            False

        BrowserGotEntityForAddressNeighbor _ _ ->
            False

        BrowserGotEntityNeighborsTable _ _ _ ->
            False

        BrowserGotAddressTxs _ _ ->
            False

        BrowserGotAddresslinkTxs _ _ ->
            False

        BrowserGotEntityAddresses _ _ ->
            False

        BrowserGotAddressForEntity _ _ ->
            False

        BrowserGotEntityAddressesForTable _ _ ->
            False

        BrowserGotEntityTxs _ _ ->
            False

        BrowserGotEntitylinkTxs _ _ ->
            False

        BrowserGotBlockTxs _ _ ->
            False

        BrowserGotAddressTags _ _ ->
            False

        BrowserGotAddressTagsTable _ _ ->
            False

        BrowserGotEntityAddressTagsTable _ _ ->
            False

        BrowserGotActorTagsTable _ _ ->
            False

        BrowserGotTx _ _ ->
            False

        BrowserGotTxUtxoAddresses _ _ _ ->
            False

        BrowserGotLabelAddressTags _ _ ->
            False

        BrowserGotTokenTxs _ _ ->
            False

        PluginMsg _ ->
            False

        TableNewState _ ->
            False

        UserClickedContextMenu ->
            False

        UserLeftContextMenu ->
            False

        UserClickedAnnotateAddress _ ->
            False

        UserClickedAnnotateEntity _ ->
            False

        InternalGraphAddedAddresses _ ->
            False

        InternalGraphAddedEntities _ ->
            False

        InternalGraphSelectedAddress _ ->
            False

        UserScrolledTable _ ->
            False

        TagSearchMsg _ ->
            False

        BrowserGotAddressElementForAnnotate _ _ ->
            False

        BrowserGotEntityElementForAnnotate _ _ ->
            False

        UserInputsTagSource _ ->
            False

        UserInputsTagCategory _ ->
            False

        UserInputsTagAbuse _ ->
            False

        UserClicksCloseTagHovercard ->
            False

        UserClicksLegend _ ->
            False

        UserClicksConfiguraton _ ->
            False

        UserClickedExport _ ->
            False

        UserClickedImport _ ->
            False

        UserClickedHighlighter _ ->
            False

        BrowserGotLegendElement _ ->
            False

        BrowserGotConfigurationElement _ ->
            False

        BrowserGotExportElement _ ->
            False

        BrowserGotImportElement _ ->
            False

        BrowserGotHighlighterElement _ ->
            False

        UserChangesCurrency _ ->
            False

        UserChangesValueDetail _ ->
            False

        UserChangesAddressLabelType _ ->
            False

        UserChangesTxLabelType _ ->
            False

        UserClickedSearch _ ->
            False

        BrowserGotEntityElementForSearch _ _ ->
            False

        UserSelectsDirection _ ->
            False

        UserSelectsCriterion _ ->
            False

        UserSelectsSearchCategory _ ->
            False

        UserInputsSearchDepth _ ->
            False

        UserInputsSearchBreadth _ ->
            False

        UserInputsSearchMaxAddresses _ ->
            False

        UserClicksCloseSearchHovercard ->
            False

        BrowserGotEntitySearchResult _ _ result ->
            List.isEmpty result |> not

        UserClickedExportGraphics _ ->
            False

        UserClickedExportTagPack _ ->
            False

        UserClickedImportTagPack ->
            False

        BrowserGotTagPackFile _ ->
            False

        BrowserReadTagPackFile _ _ ->
            False

        UserClickedExportGS _ ->
            False

        UserClickedImportGS ->
            False

        PortDeserializedGS _ ->
            False

        UserClickedUndo ->
            False

        UserClickedRedo ->
            False

        UserClickedUserTags ->
            False

        BrowserGotBulkAddresses _ _ _ ->
            False

        BrowserGotBulkAddressTags _ _ ->
            False

        BrowserGotBulkEntities _ _ _ ->
            False

        BrowserGotBulkAddressEntities _ _ _ ->
            False

        BrowserGotBulkEntityNeighbors _ _ _ ->
            False

        BrowserGotBulkAddressNeighbors _ _ _ ->
            False

        UserClickedNew ->
            False

        UserClickedHighlightTrash _ ->
            False

        UserInputsHighlightTitle _ _ ->
            False

        UserClicksHighlight _ ->
            False

        UserInputsFilterTable _ ->
            False

        UserClickedFitGraph ->
            False

        UserPressesEscape ->
            False

        UserClicksDeleteTag ->
            False

        UserClickedForceShowEntityLink _ _ ->
            False

        UserClickedShowEntityShadowLinks ->
            False

        UserClickedShowAddressShadowLinks ->
            False

        UserClickedToggleShowDatesInUserLocale ->
            False

        UserClickedTagsFlag _ ->
            False

        UserClicksDownloadCSVInTable ->
            False

        UserClickedExternalLink _ ->
            False

        UserClickedCopyToClipboard _ ->
            False

        NoOp ->
            False

        UserClickedAddressInEntityTagsTable _ _ ->
            True

        UserClickedAddressInTable _ ->
            True

        UserClickedHighlightColor _ ->
            False
