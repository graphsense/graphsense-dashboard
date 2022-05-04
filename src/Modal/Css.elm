module Modal.Css exposing (..)

import Css exposing (..)
import View.Config exposing (Config)


part : Config -> List Style
part vc =
    vc.theme.modal.part


heading : Config -> List Style
heading vc =
    vc.theme.modal.heading
