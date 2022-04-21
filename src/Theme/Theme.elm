module Theme.Theme exposing (Theme, default)

import Css exposing (Style)
import Theme.Stats as Stats exposing (Stats)


type alias Theme =
    { scaled : Float -> Float
    , logo : String
    , body : List Style
    , sectionBelowHeader : List Style
    , header : List Style
    , headerLogo : List Style
    , heading2 : List Style
    , addonsNav : List Style
    , main : List Style
    , stats : Stats
    , custom : String
    }


default : Theme
default =
    { scaled = (*) 1
    , logo = ""
    , body = []
    , sectionBelowHeader = []
    , header = []
    , heading2 = []
    , headerLogo = []
    , addonsNav = []
    , main = []
    , stats = Stats.default
    , custom = ""
    }
