module Msg.Graph exposing (..)

import Api.Data
import Api.Request.Entities
import Browser.Dom
import Bytes exposing (Bytes)
import Color
import File
import Json.Encode
import Model.Address as A
import Model.Block as B
import Model.Entity as E
import Model.Graph exposing (Deserializing, Dragging)
import Model.Graph.Browser as Browser
import Model.Graph.Coords exposing (Coords)
import Model.Graph.Id exposing (AddressId, EntityId, LinkId)
import Model.Graph.Tag as Tag
import Model.Graph.Transform as Transform
import Model.Tx as T
import Msg.Search as Search
import Plugin.Msg as Plugin
import Set exposing (Set)
import Table
import Time
import Yaml.Decode


type Msg
    = UserClickedGraph Dragging
    | UserClickedAddress AddressId
    | UserRightClickedAddress AddressId Coords
    | UserHoversAddress AddressId
    | UserClickedEntity EntityId Coords
    | UserRightClickedEntity EntityId Coords
    | UserHoversEntity EntityId
    | UserHoversEntityLink (LinkId EntityId)
    | UserClicksEntityLink (LinkId EntityId)
    | UserRightClicksEntityLink (LinkId EntityId) Coords
    | UserHoversAddressLink (LinkId AddressId)
    | UserClicksAddressLink (LinkId AddressId)
    | UserRightClicksAddressLink (LinkId AddressId) Coords
    | UserLeavesThing
    | UserClickedEntityExpandHandle EntityId Bool
    | UserClickedAddressExpandHandle AddressId Bool
    | UserClickedAddressesExpand EntityId
    | UserPushesLeftMouseButtonOnGraph Coords
    | UserMovesMouseOnGraph Coords
    | UserReleasesMouseButton
    | BrowserGotBrowserElement (Result Browser.Dom.Error Browser.Dom.Element)
    | UserWheeledOnGraph Float Float Float
    | UserPushesLeftMouseButtonOnEntity EntityId Coords
    | BrowserGotEntityNeighbors EntityId Bool Api.Data.NeighborEntities
    | BrowserGotEntityEgonet String Int Bool Api.Data.NeighborEntities
    | BrowserGotEntityEgonetForAddress String String Int Bool Api.Data.NeighborEntities
    | BrowserGotAddressEgonet AddressId Bool Api.Data.NeighborAddresses
    | BrowserGotAddressNeighbors AddressId Bool Api.Data.NeighborAddresses
    | BrowserGotAddressNeighborsTable A.Address Bool Api.Data.NeighborAddresses
    | BrowserGotNow Time.Posix
    | BrowserGotAddress Api.Data.Address
    | BrowserGotEntity Api.Data.Entity
    | BrowserGotBlock Api.Data.Block
    | BrowserGotEntityForAddress String Api.Data.Entity
    | BrowserGotEntityForAddressNeighbor
        { anchor : AddressId
        , isOutgoing : Bool
        , neighbors : List Api.Data.NeighborAddress
        }
        Api.Data.Entity
    | BrowserGotEntityNeighborsTable E.Entity Bool Api.Data.NeighborEntities
    | BrowserGotAddressTxs A.Address Api.Data.AddressTxs
    | BrowserGotAddresslinkTxs A.Addresslink Api.Data.Links
    | BrowserGotEntityAddresses EntityId Api.Data.EntityAddresses
    | BrowserGotAddressForEntity EntityId Api.Data.Address
    | BrowserGotEntityAddressesForTable E.Entity Api.Data.EntityAddresses
    | BrowserGotEntityTxs E.Entity Api.Data.AddressTxs
    | BrowserGotEntitylinkTxs E.Entitylink Api.Data.Links
    | BrowserGotBlockTxs B.Block (List Api.Data.Tx)
    | BrowserGotAddressTags A.Address Api.Data.AddressTags
    | BrowserGotAddressTagsTable A.Address Api.Data.AddressTags
    | BrowserGotEntityAddressTagsTable E.Entity Api.Data.AddressTags
    | BrowserGotTx String Api.Data.Tx
    | BrowserGotTxUtxoAddresses T.Tx Bool (List Api.Data.TxValue)
    | BrowserGotLabelAddressTags String Api.Data.AddressTags
    | BrowserGotTokenTxs T.Tx (List Api.Data.TxAccount)
    | PluginMsg Plugin.Msg
    | TableNewState Table.State
    | UserClickedContextMenu
    | UserLeftContextMenu
    | UserClickedAnnotateAddress AddressId
    | UserClickedRemoveAddress AddressId
    | UserClickedAnnotateEntity EntityId
    | UserClickedRemoveEntity EntityId
    | UserClickedRemoveAddressLink (LinkId AddressId)
    | UserClickedRemoveEntityLink (LinkId EntityId)
    | UserClickedAddressInEntityAddressesTable EntityId Api.Data.Address
    | UserClickedAddressInEntityTagsTable EntityId String
    | UserClickedAddressInTable A.Address
    | UserClickedAddressInNeighborsTable AddressId Bool Api.Data.NeighborAddress
    | UserClickedEntityInNeighborsTable EntityId Bool Api.Data.NeighborEntity
    | InternalGraphAddedAddresses (Set AddressId)
    | InternalGraphAddedEntities (Set EntityId)
    | UserScrolledTable Browser.ScrollPos
    | TagSearchMsg Search.Msg
    | BrowserGotAddressElementForAnnotate AddressId (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotEntityElementForAnnotate EntityId (Result Browser.Dom.Error Browser.Dom.Element)
    | UserInputsTagSource String
    | UserInputsTagCategory String
    | UserInputsTagAbuse String
    | UserClicksCloseTagHovercard
    | UserSubmitsTagInput
    | UserClicksLegend String
    | UserClicksConfiguraton String
    | UserClickedExport String
    | UserClickedImport String
    | UserClickedHighlighter String
    | BrowserGotLegendElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotConfigurationElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotExportElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotImportElement (Result Browser.Dom.Error Browser.Dom.Element)
    | BrowserGotHighlighterElement (Result Browser.Dom.Error Browser.Dom.Element)
    | UserChangesCurrency String
    | UserChangesAddressLabelType String
    | UserChangesTxLabelType String
    | UserClickedSearch EntityId
    | BrowserGotEntityElementForSearch EntityId (Result Browser.Dom.Error Browser.Dom.Element)
    | UserSelectsDirection String
    | UserSelectsCriterion String
    | UserSelectsSearchCategory String
    | UserInputsSearchDepth (Maybe Int)
    | UserInputsSearchBreadth (Maybe Int)
    | UserInputsSearchMaxAddresses (Maybe Int)
    | UserClicksCloseSearchHovercard
    | UserSubmitsSearchInput
    | BrowserGotEntitySearchResult EntityId Bool (List Api.Data.SearchResultLevel1)
    | UserClickedExportGraphics (Maybe Time.Posix)
    | UserClickedExportTagPack (Maybe Time.Posix)
    | UserClickedImportTagPack
    | BrowserGotTagPackFile File.File
    | BrowserReadTagPackFile String (Result Yaml.Decode.Error (List Tag.UserTag))
    | UserClickedExportGS (Maybe Time.Posix)
    | UserClickedImportGS
    | PortDeserializedGS ( String, Json.Encode.Value )
    | UserClickedUndo
    | UserClickedRedo
    | UserClickedUserTags
    | BrowserGotBulkAddresses String Deserializing (List Api.Data.Address)
    | BrowserGotBulkAddressTags String (List Api.Data.AddressTag)
    | BrowserGotBulkEntities String Deserializing (List Api.Data.Entity)
    | BrowserGotBulkAddressEntities String Deserializing (List Api.Data.Entity)
    | BrowserGotBulkEntityNeighbors String Bool (List ( Int, Api.Data.NeighborEntity ))
    | BrowserGotBulkAddressNeighbors String Bool (List ( String, Api.Data.NeighborAddress ))
    | UserClickedNew
    | UserClickedNewYes
    | UserClickedHighlightColor Color.Color
    | UserClickedHighlightTrash Int
    | UserInputsHighlightTitle Int String
    | UserClicksHighlight Int
    | UserInputsFilterTable (Maybe String)
    | UserClickedFitGraph
    | UserPressesEscape
    | UserClicksDeleteTag
    | UserClickedForceShowEntityLink (LinkId EntityId) Bool
    | UserClickedShowEntityShadowLinks
    | UserClickedShowAddressShadowLinks
    | UserPressesDelete
    | UserClickedTagsFlag EntityId
    | UserClicksDownloadCSVInTable
    | OpenExternalLink String
    | CopyToClipboard String
    | NoOp
