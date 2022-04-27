module Theme.Theme exposing (Theme, default)

import Css exposing (Style)
import Theme.Button as Button exposing (Button)
import Theme.Search as Search exposing (Search)
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
    , loadingSpinnerUrl : String
    , stats : Stats
    , search : Search
    , button : Button
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
    , loadingSpinnerUrl = ""
    , stats = Stats.default
    , search = Search.default
    , button = Button.default
    , custom = ""
    }
