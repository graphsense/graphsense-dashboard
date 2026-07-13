module Model.Dialog exposing (AddTagConfig, ClusterTagsState(..), ConfirmConfig, CustomConfig, CustomConfigWithVc, ErrorConfig, ErrorType(..), ExportArea(..), ExportConfig, ExportFormat(..), GeneralErrorConfig, InfoConfig, Model(..), OptionsConfig, Placement(..), PluginConfig, TagListConfig, TagsTab(..), defaultMsg, exportFormatToString, initExportConfig, placement)

import Api.Data
import Basics.Extra exposing (flip)
import Components.InfiniteTable as InfiniteTable
import Config.Update as Update
import Config.View exposing (Config)
import Html.Styled exposing (Html)
import Http
import Model.Pathfinder.Id exposing (Id)
import Model.Pathfinder.Selection as Selection exposing (Selection(..))
import Model.Search as Search
import Time
import View.Locale exposing (makeTimestampFilename)


{-| Vertical placement of the dialog within the overlay. `PinnedToTop` keeps
the dialog's top edge fixed, so the dialog doesn't shift when its content
grows or shrinks.
-}
type Placement
    = Centered
    | PinnedToTop


type Model msg
    = Confirm (ConfirmConfig msg)
    | Options (OptionsConfig msg)
    | Error (ErrorConfig msg)
    | Info (InfoConfig msg)
    | TagsList (TagListConfig msg)
    | AddTag (AddTagConfig msg)
    | Export (ExportConfig msg)
    | Custom (CustomConfig msg)
    | CustomWithVc (CustomConfigWithVc msg)
    | Plugin (PluginConfig msg)


type alias ConfirmConfig msg =
    { confirmText : Maybe String
    , cancelText : Maybe String
    , title : String
    , message : String
    , onYes : msg
    , onNo : msg
    }


type alias OptionsConfig msg =
    { message : String
    , options : List ( String, msg )
    , onClose : msg
    }


type alias ErrorConfig msg =
    { type_ : ErrorType
    , onOk : msg
    }


type alias InfoConfig msg =
    { info : String
    , title : Maybe String
    , variables : List String
    , onOk : msg
    }


type alias CustomConfig msg =
    { html : Html msg
    , defaultMsg : msg
    }


type alias CustomConfigWithVc msg =
    { html : Config -> Html msg
    , defaultMsg : msg
    }


type TagsTab
    = AddressTagsTab
    | ClusterTagsTab


type ClusterTagsState
    = ClusterTagsNotLoaded
    | ClusterTagsLoading
    | ClusterTagsLoaded (InfiniteTable.Model String Api.Data.AddressTag)


type alias TagListConfig msg =
    { id : Id
    , addressTagsTable : InfiniteTable.Model String Api.Data.AddressTag
    , clusterTagsState : ClusterTagsState
    , activeTab : TagsTab
    , showAddressTab : Bool
    , showClusterTab : Bool
    , hasAddressTags : Bool
    , closeMsg : msg
    }


type alias AddTagConfig msg =
    { id : Id
    , closeMsg : msg
    , addTagMsg : msg
    , search : Search.Model
    , selectedActor : Maybe ( String, String )
    , description : String
    }


type alias ExportConfig msg =
    { closeMsg : msg
    , area : ExportArea
    , hasSelections : Bool
    , keepSelectionHighlight : Bool
    , fileFormat : ExportFormat
    , filename : String
    , time : Time.Posix
    , exporting : Bool
    , transparentBackground : Bool

    -- CSV export only: when True, only the inputs/outputs currently visible on
    -- the graph are exported. Defaults to True to avoid exploding the CSV for
    -- large transactions (see initExportConfig). Ignored for non-CSV formats.
    , onlyVisibleIos : Bool
    }


type ExportArea
    = ExportAreaVisible
    | ExportAreaSelected
    | ExportAreaWhole


type ExportFormat
    = ExportFormatPDF
    | ExportFormatPNG
    | ExportFormatCSV


exportFormatToString : ExportFormat -> String
exportFormatToString format =
    case format of
        ExportFormatPDF ->
            "pdf"

        ExportFormatPNG ->
            "png"

        ExportFormatCSV ->
            "csv"


type alias PluginConfig msg =
    { defaultMsg : msg
    , placement : Placement
    }


type ErrorType
    = AddressNotFound (List String)
    | TxNotFound (List String)
    | Http String Http.Error
    | General GeneralErrorConfig


type alias GeneralErrorConfig =
    { title : String
    , message : String
    , variables : List String
    }


placement : Model msg -> Placement
placement model =
    case model of
        Plugin c ->
            c.placement

        _ ->
            Centered


defaultMsg : Model msg -> msg
defaultMsg model =
    case model of
        Options { onClose } ->
            onClose

        Confirm { onNo } ->
            onNo

        Error { onOk } ->
            onOk

        Info { onOk } ->
            onOk

        Custom c ->
            c.defaultMsg

        CustomWithVc c ->
            c.defaultMsg

        TagsList c ->
            c.closeMsg

        Export c ->
            c.closeMsg

        AddTag c ->
            c.closeMsg

        Plugin c ->
            c.defaultMsg


initExportConfig :
    Update.Config
    ->
        { selection : Selection
        , filenameBase : String
        , closeMsg : msg
        , time : Time.Posix
        }
    -> ExportConfig msg
initExportConfig uc { selection, filenameBase, closeMsg, time } =
    { closeMsg = closeMsg
    , area =
        case selection of
            Selection.MultiSelect (_ :: _) ->
                ExportAreaSelected

            _ ->
                ExportAreaWhole
    , keepSelectionHighlight = False
    , fileFormat = ExportFormatPDF
    , filename =
        makeTimestampFilename uc.locale time
            |> flip (++) (" " ++ filenameBase ++ ".pdf")
    , time = time
    , exporting = False
    , transparentBackground = False

    -- Default to exporting only visible inputs/outputs so large transactions
    -- don't blow up the CSV.
    , onlyVisibleIos = True
    , hasSelections = selection /= NoSelection
    }
