module Iknaio.DesignTokens exposing (..)

import Iknaio.DesignToken exposing (DesignToken)


addressStrokeColor : DesignToken
addressStrokeColor =
    DesignToken "address-stroke-color" "black" "white"


addressFillColor : DesignToken
addressFillColor =
    DesignToken "address-fill-color" "white" "black"


addressFontColor : DesignToken
addressFontColor =
    DesignToken "address-font-color" "black" "white"


addressFontWeight : DesignToken
addressFontWeight =
    DesignToken "address-font-weight" "100" "100"


addressSpacingToLabel : DesignToken
addressSpacingToLabel =
    DesignToken "address-spacing-to-label" "5" "5"


designTokens : List DesignToken
designTokens =
    [ addressStrokeColor
    , addressFillColor
    , addressFontColor
    , addressFontWeight
    , addressSpacingToLabel
    ]
