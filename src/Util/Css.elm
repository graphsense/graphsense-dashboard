module Util.Css exposing (zIndexMain, zIndexMainValue)

import Css exposing (Style, int, zIndex)


zIndexMain : Style
zIndexMain =
    zIndex <| int zIndexMainValue


zIndexMainValue : Int
zIndexMainValue =
    50
