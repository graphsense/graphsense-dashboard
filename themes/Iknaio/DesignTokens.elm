module Iknaio.DesignTokens exposing (..)

import Iknaio.DesignToken exposing (DesignToken)


addressStrokeColor : DesignToken
addressStrokeColor =
    DesignToken "address-stroke-color" "black" "white"


addressFillColor : DesignToken
addressFillColor =
    DesignToken "address-fill-color" "white" "black"


txStrokeColor : DesignToken
txStrokeColor =
    DesignToken "tx-stroke-color" "white" "black"


txFillColor : DesignToken
txFillColor =
    DesignToken "tx-fill-color" "black" "white"


edgeUtxoStrokeColor : DesignToken
edgeUtxoStrokeColor =
    DesignToken "edge-utxo-stroke-color" "black" "white"


addressFontColor : DesignToken
addressFontColor =
    DesignToken "address-font-color" "black" "white"


addressFontWeight : DesignToken
addressFontWeight =
    DesignToken "address-font-weight" "100" "100"


addressSpacingToLabel : DesignToken
addressSpacingToLabel =
    DesignToken "address-spacing-to-label" "5" "5"


edgeLabelFontColor : DesignToken
edgeLabelFontColor =
    DesignToken "edge-label-font-color" "black" "white"


edgeLabelFontWeight : DesignToken
edgeLabelFontWeight =
    DesignToken "edge-label-font-weight" "100" "100"


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
    ]
