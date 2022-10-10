module Update.Dialog exposing (..)

import Model.Dialog exposing (..)


confirm : ConfirmConfig msg -> Model msg
confirm =
    Confirm


options : OptionsConfig msg -> Model msg
options =
    Options
