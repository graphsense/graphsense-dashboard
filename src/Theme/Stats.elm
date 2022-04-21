module Theme.Stats exposing (Stats, default)

import Css exposing (Style)


type alias Stats =
    { root : List Style
    , currency : List Style
    , currencyHeading : List Style
    , tableWrapper : List Style
    , tableWrapperInner : List Style
    , table : List Style
    , tableRow : List Style
    , tableCellKey : List Style
    , tableCellValue : List Style
    , currencyBackground : List Style
    , currencyBackgroundPath : List Style
    }


default : Stats
default =
    { root = []
    , currency = []
    , currencyHeading = []
    , tableWrapper = []
    , tableWrapperInner = []
    , table = []
    , tableRow = []
    , tableCellKey = []
    , tableCellValue = []
    , currencyBackground = []
    , currencyBackgroundPath = []
    }
