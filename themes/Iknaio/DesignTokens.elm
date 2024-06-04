module Iknaio.DesignTokens exposing (..)

import Iknaio.DesignToken exposing (DesignToken, Variable(..))


black : Variable
black =
    Variable "black" "#000000"


white : Variable
white =
    Variable "white" "#ffffff"


red : Variable
red =
    Variable "red" "rgb(194, 141, 141)"


green : Variable
green =
    Variable "green" "rgb(18, 152, 136)"


gutter : Variable
gutter =
    Variable "5" "5"


fontThin : Variable
fontThin =
    Variable "100" "100"


thinLine : Variable
thinLine =
    Variable "2px" "2px"


addressStrokeColor : DesignToken
addressStrokeColor =
    DesignToken "address-stroke-color" black white


addressFillColor : DesignToken
addressFillColor =
    DesignToken "address-fill-color" white black


txStrokeColor : DesignToken
txStrokeColor =
    DesignToken "tx-stroke-color" white black


txFillColor : DesignToken
txFillColor =
    DesignToken "tx-fill-color" black white


edgeUtxoStrokeColor : DesignToken
edgeUtxoStrokeColor =
    DesignToken "edge-utxo-stroke-color" black white


edgeUtxoStrokeWidth : DesignToken
edgeUtxoStrokeWidth =
    DesignToken "edge-utxo-stroke-width" thinLine thinLine


edgeUtxoOutStrokeColor : DesignToken
edgeUtxoOutStrokeColor =
    DesignToken "edge-utxo-out-stroke-color" green green


edgeUtxoInStrokeColor : DesignToken
edgeUtxoInStrokeColor =
    DesignToken "edge-utxo-in-stroke-color" red red


addressFontColor : DesignToken
addressFontColor =
    DesignToken "address-font-color" black white


addressFontWeight : DesignToken
addressFontWeight =
    DesignToken "address-font-weight" fontThin fontThin


addressSpacingToLabel : DesignToken
addressSpacingToLabel =
    DesignToken "address-spacing-to-label" gutter gutter


edgeLabelFontColor : DesignToken
edgeLabelFontColor =
    DesignToken "edge-label-font-color" black white


edgeLabelFontWeight : DesignToken
edgeLabelFontWeight =
    DesignToken "edge-label-font-weight" fontThin fontThin


designTokens : List DesignToken
designTokens =
    [ addressStrokeColor
    , addressFillColor
    , addressFontColor
    , addressFontWeight
    , addressSpacingToLabel
    , txFillColor
    , txStrokeColor
    , edgeUtxoStrokeColor
    , edgeLabelFontColor
    , edgeLabelFontWeight
    , edgeUtxoOutStrokeColor
    , edgeUtxoInStrokeColor
    , edgeUtxoStrokeWidth
    ]
