module Model.Dialog exposing (AddTagConfig, ConfirmConfig, CustomConfig, CustomConfigWithVc, ErrorConfig, ErrorType(..), GeneralErrorConfig, InfoConfig, Model(..), OptionsConfig, PluginConfig, TagListConfig, defaultMsg)

import Api.Data
import Config.View exposing (Config)
import Html.Styled exposing (Html)
import Http
import Model.Graph.Table exposing (Table)
import Model.Pathfinder.Id exposing (Id)
import Model.Search as Search


type Model msg
    = Confirm (ConfirmConfig msg)
    | Options (OptionsConfig msg)
    | Error (ErrorConfig msg)
    | Info (InfoConfig msg)
    | TagsList (TagListConfig msg)
    | AddTag (AddTagConfig msg)
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
    , search : Search.Model
    , selectedActor : Maybe ( String, String )
    , description : String
    }


type alias PluginConfig msg =
    { defaultMsg : msg
    }


type ErrorType
    = AddressNotFound (List String)
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

        AddTag c ->
            c.closeMsg

        Plugin c ->
            c.defaultMsg
