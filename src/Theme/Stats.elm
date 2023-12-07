module Theme.Stats exposing (Stats, default)

import Css exposing (Style)


type alias Stats =
    { root : List Style
    , stats : List Style
    , currency : Bool -> List Style
    , currencyHeading : Bool -> List Style
    , tableWrapper : List Style
    , tableWrapperInner : List Style
    , table : List Style
    , tableRow : List Style
    , tableCellKey : List Style
    , tableCellValue : List Style
    , currencyBackground : List Style
    , currencyBackgroundPath : List Style
    , loadingSpinner : List Style
    , tokenBadgeStyle : Bool -> List Style
    }


default : Stats
default =
    { root = []
    , stats = []
    , currency = \_ -> []
    , currencyHeading = \_ -> []
    , tableWrapper = []
    , tableWrapperInner = []
    , table = []
    , tableRow = []
    , tableCellKey = []
    , tableCellValue = []
    , currencyBackground = []
    , currencyBackgroundPath = []
    , loadingSpinner = []
    , tokenBadgeStyle = \_ -> []
    }
