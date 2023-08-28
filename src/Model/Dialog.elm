module Model.Dialog exposing (..)

import Html.Styled exposing (Html)


type Model msg
    = Confirm (ConfirmConfig msg)
    | Options (OptionsConfig msg)
    | Error (ErrorConfig msg)
    | Info (InfoConfig msg)


type alias ConfirmConfig msg =
    { message : String
    , onYes : msg
    , onNo : msg
    }


type alias OptionsConfig msg =
    { message : String
    , options : List ( String, msg )
    }


type alias ErrorConfig msg =
    { type_ : ErrorType
    , onOk : msg
    }


type alias InfoConfig msg =
    { info : String
    , variables : List String
    , onOk : msg
    }


type ErrorType
    = AddressNotFound (List String)
