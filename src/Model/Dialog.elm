module Model.Dialog exposing (..)


type Model msg
    = Confirm (ConfirmConfig msg)
    | Options (OptionsConfig msg)


type alias ConfirmConfig msg =
    { message : String
    , onYes : msg
    , onNo : msg
    }


type alias OptionsConfig msg =
    { message : String
    , options : List ( String, msg )
    }
