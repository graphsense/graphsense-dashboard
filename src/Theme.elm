module Theme exposing (Colors, Theme)

import Css exposing (Color, Style, batch)


type alias Colors =
    { black : Color
    , greyDarkest : Color
    , greyDarker : Color
    , greyDark : Color
    , grey : Color
    , greyLight : Color
    , greyLighter : Color
    , greyLightest : Color
    , white : Color
    , brandText : Color
    , brandDarker : Color
    , brandDark : Color
    , brandBase : Color
    , brandLight : Color
    , brandLighter : Color
    , brandLightest : Color
    , brandRed : Color
    , brandRedLight : Color
    , brandWhite : Color
    }


type alias Theme =
    { scaled : Float -> Float
    , logo : String
    , body : Style
    , header : Style
    , headerLogo : Style
    , heading2 : Style
    , addonsNav : Style
    , main : Style
    , stats :
        { root : Style
        , currency : Style
        , currencyHeading : Style
        , tableWrapper : Style
        , tableWrapperInner : Style
        , table : Style
        , tableRow : Style
        , tableCellKey : Style
        , tableCellValue : Style
        , currencyBackground : Style
        , currencyBackgroundPath : Style
        }
    , custom : String
    }
