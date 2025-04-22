module Util.Css exposing (overrideBlack, zIndexMain, zIndexMainValue)

import Css exposing (Style, int, zIndex)
import Theme.Colors


zIndexMain : Style
zIndexMain =
    zIndex <| int zIndexMainValue


zIndexMainValue : Int
zIndexMainValue =
    50


overrideBlack : String -> Style
overrideBlack =
    -- that's a hacky workaround to apply color overrides
    Css.property Theme.Colors.sidebarNeutral_name
