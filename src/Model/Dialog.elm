module Model.Dialog exposing (AddTagConfig, ConfirmConfig, CustomConfig, CustomConfigWithVc, ErrorConfig, ErrorType(..), ExportArea(..), ExportConfig, ExportFormat(..), GeneralErrorConfig, InfoConfig, Model(..), OptionsConfig, PluginConfig, TagListConfig, defaultMsg, exportFormatToString, initExportConfig)

import Api.Data
import Basics.Extra exposing (flip)
import Components.Table exposing (Table)
import Config.Update as Update
import Config.View exposing (Config)
import Html.Styled exposing (Html)
import Http
import Model.Pathfinder.Id exposing (Id)
import Model.Search as Search
import Time
import View.Locale exposing (makeTimestampFilename)


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


type alias TagListConfig msg =
    { id : Id
    , tagsTable : Table Api.Data.AddressTag
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
    , keepSelectionHighlight : Bool
    , fileFormat : ExportFormat
    , filename : String
    , time : Time.Posix
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


initExportConfig : Update.Config -> String -> msg -> Time.Posix -> ExportConfig msg
initExportConfig uc filenameBase closeMsg time =
    { closeMsg = closeMsg
    , area = ExportAreaVisible
    , keepSelectionHighlight = True
    , fileFormat = ExportFormatPDF
    , filename =
        makeTimestampFilename uc.locale time
            |> flip (++) (" " ++ filenameBase ++ ".pdf")
    , time = time
    }
