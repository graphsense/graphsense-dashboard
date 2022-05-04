module Theme.Theme exposing (Theme, default)

import Css exposing (Style)
import Theme.Button as Button exposing (Button)
import Theme.Hovercard as Hovercard exposing (Hovercard)
import Theme.Modal as Modal exposing (Modal)
import Theme.Search as Search exposing (Search)
import Theme.Stats as Stats exposing (Stats)
import Theme.User as User exposing (User)


type alias Theme =
    { scaled : Float -> Float
    , logo : String
    , body : List Style
    , sectionBelowHeader : List Style
    , header : List Style
    , headerLogo : List Style
    , heading2 : List Style
    , input : List Style
    , addonsNav : List Style
    , main : List Style
    , loadingSpinnerUrl : String
    , stats : Stats
    , search : Search
    , button : Button
    , tool : List Style
    , modal : Modal
    , user : User
    , hovercard : Hovercard
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
    , input = []
    , headerLogo = []
    , addonsNav = []
    , main = []
    , loadingSpinnerUrl = ""
    , stats = Stats.default
    , search = Search.default
    , button = Button.default
    , tool = []
    , hovercard = Hovercard.default
    , modal = Modal.default
    , user = User.default
    , custom = ""
    }
