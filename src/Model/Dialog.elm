module Model.Dialog exposing (..)


type Model msg
    = Confirm (ConfirmConfig msg)
    | Options (OptionsConfig msg)
    | Error (ErrorConfig msg)


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


type ErrorType
    = AddressNotFound (List String)
