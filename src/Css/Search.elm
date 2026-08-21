module Css.Search exposing (resultLine, resultLineHighlighted)

import Config.View exposing (Config)
import Css exposing (..)


resultLine : Config -> List Style
resultLine vc =
    cursor pointer
        :: overflowX hidden
        :: vc.theme.search.resultLine vc.lightmode


resultLineHighlighted : Config -> List Style
resultLineHighlighted vc =
    vc.theme.search.resultLineHighlighted vc.lightmode
