module Iknaio.ColorScheme exposing (color0, color1, color2, color3, color4, color5, color6, color7, color8, color9, colorScheme, colorSchemePathfinder, defaultColor)

import Color exposing (Color, rgb255)


defaultColor : Color
defaultColor =
    rgb255 138 138 138


color0 : Color
color0 =
    rgb255 228 148 68


color1 : Color
color1 =
    rgb255 209 97 93


color2 : Color
color2 =
    rgb255 133 182 178


color3 : Color
color3 =
    rgb255 106 159 88


color4 : Color
color4 =
    rgb255 231 202 96


color5 : Color
color5 =
    rgb255 168 124 159


color6 : Color
color6 =
    rgb255 241 162 169


color7 : Color
color7 =
    rgb255 87 120 164


color8 : Color
color8 =
    rgb255 150 118 98


color9 : Color
color9 =
    rgb255 184 176 172


colorScheme : List Color
colorScheme =
    [ color0
    , color1
    , color2
    , color3
    , color4
    , color5
    , color6
    , color7
    , color8
    , color9
    ]


color2Pathfinder : Color
color2Pathfinder =
    rgb255 59 114 71



-- color0 = 228 148 68
-- color1 = 133 182 178
-- color2 = 59 114 71
-- color3 = 231 202 96
-- color4 = 168 124 159
-- color5 = 241 162 169
-- color6 = 87 120 164
-- color7 = 150 118 98
-- color8 = 106 159 88
-- color9 = 184 176 172


colorSchemePathfinder : List Color
colorSchemePathfinder =
    [ color0
    , color2
    , color2Pathfinder
    , color4
    , color5
    , color6
    , color7
    , color8
    , color3
    , color9
    ]
