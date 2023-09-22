module Config.Popup exposing (..)

import Draggable
import Model exposing (Msg(..))


config : Draggable.Config String Msg
config =
    Draggable.basicConfig UserDragsPopup
