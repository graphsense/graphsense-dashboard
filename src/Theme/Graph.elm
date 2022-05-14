module Theme.Graph exposing (Graph, default)

import Color exposing (Color)
import Css exposing (Style)
import Model.Graph exposing (NodeType)


type alias Graph =
    { root : List Style
    , addressFlags : List Style
    , addressLabel : List Style
    , addressRect : List Style
    , addressFrame : List Style
    , addressRoot : List Style
    , entityFlags : List Style
    , entityLabel : List Style
    , entityCurrency : List Style
    , entityAddressesCount : List Style
    , entityRect : List Style
    , entityFrame : List Style
    , entityRoot : List Style
    , entityLink : List Style
    , entityLinkThickness : Float
    , expandHandle : NodeType -> List Style
    , expandHandlePath : NodeType -> List Style
    , expandHandleText : NodeType -> List Style
    , nodeSeparatorToExpandHandle : NodeType -> List Style
    , graphRoot : List Style
    , svgRoot : List Style
    , navbar : List Style
    , navbarLeft : List Style
    , navbarRight : List Style
    , tool : List Style
    , colorScheme : List Color
    , lightnessFactor :
        { entity : Float
        , address : Float
        }
    , saturationFactor :
        { entity : Float
        , address : Float
        }
    , defaultColor : Color
    }


default : Graph
default =
    { root = []
    , addressFlags = []
    , addressLabel = []
    , addressRect = []
    , addressFrame = []
    , addressRoot = []
    , entityFlags = []
    , entityLabel = []
    , entityCurrency = []
    , entityAddressesCount = []
    , entityRect = []
    , entityFrame = []
    , entityRoot = []
    , entityLink = []
    , entityLinkThickness = 1
    , expandHandle = always []
    , expandHandlePath = always []
    , expandHandleText = always []
    , nodeSeparatorToExpandHandle = always []
    , graphRoot = []
    , svgRoot = []
    , navbar = []
    , navbarLeft = []
    , navbarRight = []
    , tool = []
    , colorScheme = []
    , lightnessFactor =
        { entity = 1
        , address = 1
        }
    , saturationFactor =
        { entity = 1
        , address = 1
        }
    , defaultColor = Color.rgb 255 255 255
    }
