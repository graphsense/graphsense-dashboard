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
    18


addressesCountHeight : Float
addressesCountHeight =
    16


maxExpandableNeighbors : Int
maxExpandableNeighbors =
    25


maxExpandableAddresses : Int
maxExpandableAddresses =
    20


txMaxWidth : Float
txMaxWidth =
    7


arrowHeight : Float
arrowHeight =
    txMaxWidth


arrowWidth : Float
arrowWidth =
    arrowHeight


linkLabelHeight : Float
linkLabelHeight =
    12


entityMinHeight : Float
entityMinHeight =
    (2 * padding)
        + labelHeight
        + addressesCountHeight
        + padding


type AddressLabelType
    = ID
    | Balance
    | Tag


type TxLabelType
    = NoTxs
    | Value


type alias Config =
    { addressLabelType : AddressLabelType
    , txLabelType : TxLabelType
    , maxLettersPerLabelRow : Int
    , colors : Dict String Color
    }


default : Config
default =
    { addressLabelType = ID
    , txLabelType = Value
    , maxLettersPerLabelRow = 19
    , colors = Dict.empty
    }
