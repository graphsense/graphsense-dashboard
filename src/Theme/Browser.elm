module Theme.Browser exposing (Browser, default)

import Color exposing (Color)
import Css exposing (Style)


type alias Browser =
    { root : List Style
    , frame : Bool -> Bool -> List Style
    , propertyBoxRoot : Bool -> List Style
    , propertyBoxTable : List Style
    , propertyBoxNote : Bool -> List Style
    , propertyBoxRow : Bool -> List Style
    , propertyBoxKey : List Style
    , propertyBoxValue : List Style
    , propertyBoxValueInner : List Style
    , propertyBoxEntityId : Bool -> List Style
    , propertyBoxRule : List Style
    , propertyBoxIncomingTxs : Bool -> List Style
    , propertyBoxOutgoingTxs : Bool -> List Style
    , propertyBoxUsageTimestamp : List Style
    , propertyBoxUsageRelative : List Style
    , propertyBoxActivityPeriod : List Style
    , propertyBoxTableLink : Bool -> Bool -> List Style
    , copyLink : Bool -> Bool -> List Style
    , loadingSpinner : List Style
    , valueCell : List Style
    , currencyCell : List Style
    , longIdentifier : List Style
    }


default : Browser
default =
    { root = []
    , frame = \_ _ -> []
    , propertyBoxRoot = \_ -> []
    , propertyBoxTable = []
    , propertyBoxNote = \_ -> []
    , propertyBoxRow = \_ -> []
    , propertyBoxKey = []
    , propertyBoxValue = []
    , propertyBoxValueInner = []
    , propertyBoxEntityId = \_ -> []
    , propertyBoxRule = []
    , propertyBoxIncomingTxs = \_ -> []
    , propertyBoxOutgoingTxs = \_ -> []
    , propertyBoxUsageTimestamp = []
    , propertyBoxUsageRelative = []
    , propertyBoxActivityPeriod = []
    , propertyBoxTableLink = \_ _ -> []
    , copyLink = \_ _ -> []
    , loadingSpinner = []
    , valueCell = []
    , currencyCell = []
    , longIdentifier = []
    }
