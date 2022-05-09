module Config.Graph exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)


entityWidth : Float
entityWidth =
    190


layerMargin : Float
layerMargin =
    300


addressWidth : Float
addressWidth =
    entityWidth - 2 * padding - 2 * expandHandleWidth


addressHeight : Float
addressHeight =
    50


expandHandleWidth : Float
expandHandleWidth =
    15


padding : Float
padding =
    10


labelHeight : Float
labelHeight =
    20


addressesCountHeight : Float
addressesCountHeight =
    16


type AddressLabelType
    = ID
    | Balance
    | Tag


type alias Config =
    { maxLettersPerLabelRow : Int
    , addressLabelType : AddressLabelType
    , colors : Dict String Color
    }


default : Config
default =
    { addressLabelType = ID
    , maxLettersPerLabelRow = 8
    , colors = Dict.empty
    }
