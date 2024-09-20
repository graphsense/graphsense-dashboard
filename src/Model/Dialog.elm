module Model.Dialog exposing (ConfirmConfig, ErrorConfig, ErrorType(..), GeneralErrorConfig, InfoConfig, Model(..), OptionsConfig, defaultMsg)

import Http


type Model msg
    = Confirm (ConfirmConfig msg)
    | Options (OptionsConfig msg)
    | Error (ErrorConfig msg)
    | Info (InfoConfig msg)


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
