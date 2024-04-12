module Iknaio.DesignTokens exposing (..)

import Iknaio.DesignToken exposing (DesignToken)


addressStroke : DesignToken
addressStroke =
    DesignToken "address-fill" "black" "white"


addressFill : DesignToken
addressFill =
    DesignToken "address-stroke" "white" "black"


designTokens : List DesignToken
designTokens =
    [ addressStroke
    , addressFill
    ]
