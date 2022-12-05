module Lense exposing (..)

import Lense.Dialog as Dialog
import Model exposing (..)
import Model.Dialog as Dialog
import Monocle.Compose exposing (optionalWithOptional)
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)


model2Dialog : Optional (Model key) (Dialog.Model Msg)
model2Dialog =
    { getOption = .dialog
    , set = \dialog model -> { model | dialog = Just dialog }
    }


model2Dialog2ConfirmConfig : Optional (Model key) (Dialog.ConfirmConfig Msg)
model2Dialog2ConfirmConfig =
    model2Dialog
        |> optionalWithOptional Dialog.dialog2ConfirmConfig
