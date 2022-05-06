module Config.Graph exposing (..)


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


noAddressesLabelHeight : Float
noAddressesLabelHeight =
    16


type AddressLabelType
    = ID
    | Balance
    | Tag


type alias Config =
    { maxLettersPerLabelRow : Int
    , addressLabelType : AddressLabelType
    }


default : Config
default =
    { addressLabelType = ID
    , maxLettersPerLabelRow = 8
    }
