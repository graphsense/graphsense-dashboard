module Lense.Dialog exposing (..)

import Model.Dialog exposing (..)
import Monocle.Optional exposing (Optional)


dialog2ConfirmConfig : Optional (Model msg) (ConfirmConfig msg)
dialog2ConfirmConfig =
    { getOption =
        \dialog ->
            case dialog of
                Confirm conf ->
                    Just conf

                _ ->
                    Nothing
    , set = \confirm _ -> Confirm confirm
    }
