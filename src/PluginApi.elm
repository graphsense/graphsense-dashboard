module PluginApi exposing (ComponentsInfiniteTableConfig, ComponentsInfiniteTableModel, ComponentsInfiniteTableMsg, ComponentsInfiniteTableTableConfig, ComponentsPagedTableMsg, ComponentsTableFilter, ComponentsTableTable, ComponentsTooltipConfig, ComponentsTooltipModel, ComponentsTooltipMsg, ConfigUpdateConfig, ConfigViewConfig, CssTableStyles, EffectApiEffect, ModelAddressAddress, ModelCurrencyAssetIdentifier, ModelDialogPlacement, ModelEntityEntity, ModelGraphCoordsCoords, ModelPathfinderAddressAddress, ModelPathfinderIdId, RoutePathfinderAddressHopType, RoutePathfinderPathHopType, UtilThemedSelectBoxModel, UtilThemedSelectBoxMsg, ViewPathfinderContextMenuItemContextMenuItem, surface)

{-| The core API that the dashboard plugins depend on.

This module creates no behaviour. It exists so that dead-code analysis is
correct **without a plugin checkout**.

`elm.json` is generated and gains one `source-directories` entry per plugin
registered in `config/Config.elm`, so elm-review only sees the plugins that
happen to be present. CI builds from `config/Config.elm.tmp`, which registers
none, and in that checkout every core export that only a plugin uses looks
dead. Referencing those exports here keeps them alive for the analyser
whichever plugins are checked out.

So this doubles as the plugin contract: if a symbol appears below, a plugin
repository may depend on it. `tests/PluginApiTest.elm` imports this module, so
`make test` fails if one of them is deleted or renamed. Signature changes are
_not_ caught -- `ref` accepts any type.

`PluginInterface.*` is deliberately absent -- it is the designated plugin
interface already, and the dead-code config exempts it by name.

`make plugin-api` only ever _adds_ to this list. A plugin missing from the
working tree -- routine, since it is faster to develop without the ones you are
not touching -- therefore cannot shrink it. Removing entries is `make
plugin-api-prune`, which requires every registered plugin to be present.

-}

import Components.InfiniteTable
import Components.PagedTable
import Components.Table
import Components.Tooltip
import Config.Update
import Config.View
import Css.Pathfinder
import Css.Table
import Effect.Api
import Log
import Model.Address
import Model.Currency
import Model.Dialog
import Model.Direction
import Model.Entity
import Model.Graph.Coords
import Model.Locale
import Model.Notification
import Model.Pathfinder.Address
import Model.Pathfinder.Id
import Model.Pathfinder.Tx
import Route
import Route.Pathfinder
import Update.Dialog
import Update.Search
import Util
import Util.Checkbox
import Util.Css
import Util.Data
import Util.Graph
import Util.Pathfinder.TagSummary
import Util.TextDimensions
import Util.ThemedSelectBox
import Util.Tooltip
import Util.View
import Util.View.Loadingspinner
import View.Autocomplete
import View.Button
import View.Controls
import View.CurrencyMeta
import View.Graph.Table
import View.Locale
import View.Pathfinder.ContextMenuItem
import View.Pathfinder.InfiniteTable
import View.Sidebar



-- TYPES USED BY PLUGINS
--
-- Re-declared rather than referenced: a type cannot be passed as a value.
-- Names are module-qualified because `Model`, `Msg` and `Config` collide.


type alias ComponentsInfiniteTableConfig a b =
    Components.InfiniteTable.Config a b


type alias ComponentsInfiniteTableModel a b =
    Components.InfiniteTable.Model a b


type alias ComponentsInfiniteTableMsg =
    Components.InfiniteTable.Msg


type alias ComponentsInfiniteTableTableConfig a b =
    Components.InfiniteTable.TableConfig a b


type alias ComponentsPagedTableMsg =
    Components.PagedTable.Msg


type alias ComponentsTableFilter a =
    Components.Table.Filter a


type alias ComponentsTableTable a =
    Components.Table.Table a


type alias ComponentsTooltipConfig a b =
    Components.Tooltip.Config a b


type alias ComponentsTooltipModel a =
    Components.Tooltip.Model a


type alias ComponentsTooltipMsg a =
    Components.Tooltip.Msg a


type alias ConfigUpdateConfig =
    Config.Update.Config


type alias ConfigViewConfig =
    Config.View.Config


type alias CssTableStyles =
    Css.Table.Styles


type alias EffectApiEffect a =
    Effect.Api.Effect a


type alias ModelAddressAddress =
    Model.Address.Address


type alias ModelCurrencyAssetIdentifier =
    Model.Currency.AssetIdentifier


type alias ModelDialogPlacement =
    Model.Dialog.Placement


type alias ModelEntityEntity =
    Model.Entity.Entity


type alias ModelGraphCoordsCoords =
    Model.Graph.Coords.Coords


type alias ModelPathfinderAddressAddress =
    Model.Pathfinder.Address.Address


type alias ModelPathfinderIdId =
    Model.Pathfinder.Id.Id


type alias RoutePathfinderAddressHopType =
    Route.Pathfinder.AddressHopType


type alias RoutePathfinderPathHopType =
    Route.Pathfinder.PathHopType


type alias UtilThemedSelectBoxModel a =
    Util.ThemedSelectBox.Model a


type alias UtilThemedSelectBoxMsg a =
    Util.ThemedSelectBox.Msg a


type alias ViewPathfinderContextMenuItemContextMenuItem a =
    View.Pathfinder.ContextMenuItem.ContextMenuItem a


{-| Mentions a value without caring what its type is.
-}
ref : a -> ()
ref _ =
    ()


{-| Every core value, function and type constructor a plugin references.
-}
surface : List ()
surface =
    -- Components.InfiniteTable
    [ ref Components.InfiniteTable.abort
    , ref Components.InfiniteTable.appendData
    , ref Components.InfiniteTable.config
    , ref Components.InfiniteTable.getCurrentData
    , ref Components.InfiniteTable.getPage
    , ref Components.InfiniteTable.gotoFirstPage
    , ref Components.InfiniteTable.init
    , ref Components.InfiniteTable.isEmpty
    , ref Components.InfiniteTable.isLoading
    , ref Components.InfiniteTable.loadFirstPage
    , ref Components.InfiniteTable.removeItem
    , ref Components.InfiniteTable.setCountable
    , ref Components.InfiniteTable.setData
    , ref Components.InfiniteTable.setTriggerOffset
    , ref Components.InfiniteTable.sortBy
    , ref Components.InfiniteTable.update
    , ref Components.InfiniteTable.updateItem
    , ref Components.InfiniteTable.view
    , ref Components.InfiniteTable.withFetch

    -- Components.Tooltip
    , ref Components.Tooltip.attributes
    , ref Components.Tooltip.defaultConfig
    , ref Components.Tooltip.init
    , ref Components.Tooltip.perform
    , ref Components.Tooltip.update
    , ref Components.Tooltip.view
    , ref Components.Tooltip.withKeepOpenOnHover
    , ref Components.Tooltip.withMaxHeight
    , ref Components.Tooltip.withMinWidth

    -- Config.View
    , ref Config.View.getConceptName

    -- Css.Pathfinder
    , ref Css.Pathfinder.fullWidth

    -- Css.Table
    , ref Css.Table.styles

    -- Effect.Api
    , ref Effect.Api.BulkGetAddressEntityEffect
    , ref Effect.Api.GetAddressEffect
    , ref Effect.Api.GetAddressTagSummaryEffect
    , ref Effect.Api.GetAddressTxsEffect
    , ref Effect.Api.GetEntityForAddressEffect
    , ref Effect.Api.GetTxEffect
    , ref Effect.Api.SearchEffect
    , ref Effect.Api.defaultSearchConfig

    -- Log
    , ref Log.log

    -- Model.Address
    , ref Model.Address.fromPathfinderId

    -- Model.Currency
    , ref Model.Currency.asset

    -- Model.Dialog
    , ref Model.Dialog.Centered
    , ref Model.Dialog.Confirm
    , ref Model.Dialog.CustomWithVc
    , ref Model.Dialog.PinnedToTop
    , ref Model.Dialog.Plugin

    -- Model.Direction
    , ref Model.Direction.Incoming

    -- Model.Locale
    , ref Model.Locale.getFiatValue

    -- Model.Notification
    , ref Model.Notification.errorDefault
    , ref Model.Notification.fromHttpErrorWithMoreInfo
    , ref Model.Notification.infoDefault
    , ref Model.Notification.map
    , ref Model.Notification.successDefault

    -- Model.Pathfinder.Address
    , ref Model.Pathfinder.Address.expandAllowed
    , ref Model.Pathfinder.Address.isSharedService

    -- Model.Pathfinder.Id
    , ref Model.Pathfinder.Id.id
    , ref Model.Pathfinder.Id.network
    , ref Model.Pathfinder.Id.toString

    -- Model.Pathfinder.Tx
    , ref Model.Pathfinder.Tx.getTxIdForAddressTx

    -- Route
    , ref Route.pathfinderRoute
    , ref Route.pluginRoute
    , ref Route.toUrl

    -- Route.Pathfinder
    , ref Route.Pathfinder.AddressHop
    , ref Route.Pathfinder.NormalAddress
    , ref Route.Pathfinder.PerpetratorAddress
    , ref Route.Pathfinder.Root
    , ref Route.Pathfinder.TxHop
    , ref Route.Pathfinder.VictimAddress
    , ref Route.Pathfinder.addressRoute
    , ref Route.Pathfinder.pathRoute

    -- Update.Dialog
    , ref Update.Dialog.confirm
    , ref Update.Dialog.httpError
    , ref Update.Dialog.info

    -- Update.Search
    , ref Update.Search.filterByPrefix

    -- Util
    , ref Util.andWithCmd

    -- Util.Checkbox
    , ref Util.Checkbox.bigSize
    , ref Util.Checkbox.checkbox
    , ref Util.Checkbox.smallSize
    , ref Util.Checkbox.stateFromBool

    -- Util.Css
    , ref Util.Css.alignItemsStretch
    , ref Util.Css.overrideBlack
    , ref Util.Css.zIndexMainValue

    -- Util.Data
    , ref Util.Data.isAccountLike
    , ref Util.Data.timestampToPosix

    -- Util.Graph
    , ref Util.Graph.decodeCoords

    -- Util.Pathfinder.TagSummary
    , ref Util.Pathfinder.TagSummary.isExchangeNode

    -- Util.TextDimensions
    , ref Util.TextDimensions.estimateTextWidth

    -- Util.ThemedSelectBox
    , ref Util.ThemedSelectBox.Selected
    , ref Util.ThemedSelectBox.defaultConfig
    , ref Util.ThemedSelectBox.defaultConfigHtml
    , ref Util.ThemedSelectBox.init
    , ref Util.ThemedSelectBox.update
    , ref Util.ThemedSelectBox.updateOptions
    , ref Util.ThemedSelectBox.view
    , ref Util.ThemedSelectBox.withAttributes

    -- Util.Tooltip
    , ref Util.Tooltip.tooltipProperties
    , ref Util.Tooltip.tooltipRow
    , ref Util.Tooltip.tooltipRowCustomValue

    -- Util.View
    , ref Util.View.colorToHex
    , ref Util.View.conditionalHide
    , ref Util.View.copyIconPathfinder
    , ref Util.View.copyIconPathfinderFixed
    , ref Util.View.fixFillRule
    , ref Util.View.fullWidthCss
    , ref Util.View.inputFieldStyles
    , ref Util.View.none
    , ref Util.View.onClickWithStop
    , ref Util.View.pointer
    , ref Util.View.truncateLongIdentifier
    , ref Util.View.truncateLongIdentifierWithLengths

    -- Util.View.Loadingspinner
    , ref Util.View.Loadingspinner.html
    , ref Util.View.Loadingspinner.svg

    -- View.Autocomplete
    , ref View.Autocomplete.dropdown
    , ref View.Autocomplete.dropdownStyled

    -- View.Button
    , ref View.Button.button
    , ref View.Button.buttonWithAttributes
    , ref View.Button.defaultConfig
    , ref View.Button.linkButtonBlue
    , ref View.Button.linkButtonUnderlinedGray
    , ref View.Button.primaryButton
    , ref View.Button.primaryButtonGreen
    , ref View.Button.secondaryButton
    , ref View.Button.threeDots

    -- View.Controls
    , ref View.Controls.radioSmall

    -- View.CurrencyMeta
    , ref View.CurrencyMeta.getHumanReadableName

    -- View.Graph.Table
    , ref View.Graph.Table.customizations
    , ref View.Graph.Table.htmlColumn
    , ref View.Graph.Table.htmlColumnWithSorter
    , ref View.Graph.Table.intColumn
    , ref View.Graph.Table.noTools
    , ref View.Graph.Table.simpleTheadHelp
    , ref View.Graph.Table.stringColumn
    , ref View.Graph.Table.table

    -- View.Locale
    , ref View.Locale.coin
    , ref View.Locale.coinWithoutCode
    , ref View.Locale.date
    , ref View.Locale.durationToStringWithPrecision
    , ref View.Locale.fiat
    , ref View.Locale.fiatWithoutCode
    , ref View.Locale.httpErrorToString
    , ref View.Locale.intWithoutValueDetailFormatting
    , ref View.Locale.interpolated
    , ref View.Locale.interpolatedMarkdown
    , ref View.Locale.percentage
    , ref View.Locale.string
    , ref View.Locale.text
    , ref View.Locale.time
    , ref View.Locale.timestamp
    , ref View.Locale.timestampDateTimeUniform
    , ref View.Locale.timestampDateUniform
    , ref View.Locale.timestampTimeUniform
    , ref View.Locale.title
    , ref View.Locale.titleCase

    -- View.Pathfinder.ContextMenuItem
    , ref View.Pathfinder.ContextMenuItem.init
    , ref View.Pathfinder.ContextMenuItem.init2
    , ref View.Pathfinder.ContextMenuItem.initLink2
    , ref View.Pathfinder.ContextMenuItem.setAvailableOnLiteNetworks
    , ref View.Pathfinder.ContextMenuItem.setDisabled
    , ref View.Pathfinder.ContextMenuItem.view

    -- View.Pathfinder.InfiniteTable
    , ref View.Pathfinder.InfiniteTable.loadingPlaceholderAbove
    , ref View.Pathfinder.InfiniteTable.loadingPlaceholderBelow

    -- View.Sidebar
    , ref View.Sidebar.sidebarMenuItem
    ]
