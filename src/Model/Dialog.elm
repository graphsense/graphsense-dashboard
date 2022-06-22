module Model.Dialog exposing (..)


type Model msg
    = Confirm (ConfirmConfig msg)


type alias ConfirmConfig msg =
    { message : String
    , onYes : msg
    , onNo : msg
    }
