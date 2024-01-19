module Util.Css exposing (..)

import Css exposing (Style, int, zIndex)


zIndexMain : Style
zIndexMain =
    zIndex <| int zIndexMainValue


zIndexMainValue : Int
zIndexMainValue =
    50
