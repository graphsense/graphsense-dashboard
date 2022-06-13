module Theme.Graph exposing (Graph, default)

import Color exposing (Color)
import Css exposing (Style)
import Model.Graph exposing (NodeType)


type alias Graph =
    { root : List Style
    , addressFlags : List Style
    , abuseFlag : List Style
    , flagsGap : Float
    , addressLabel : List Style
    , addressRect : List Style
    , nodeFrame : NodeType -> Bool -> List Style
    , addressRoot : List Style
    , entityFlags : List Style
    , entityLabel : List Style
    , entityCurrency : List Style
    , entityAddressesCount : List Style
    , entityRect : List Style
    , entityRoot : List Style
    , link : NodeType -> Bool -> List Style
    , linkThickness : Float
    , linkColorFaded : Color
    , linkColorStrong : Color
    , linkColorSelected : Color
    , linkLabel : Bool -> List Style
    , linkLabelBox : Bool -> List Style
    , expandHandle : NodeType -> List Style
    , expandHandlePath : NodeType -> Bool -> List Style
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
    , searchTextarea : List Style
    , toolbox : Bool -> List Style
    , legendItem : List Style
    , legendItemColor : List Style
    , legendItemTitle : List Style
    , radio : List Style
    , radioInput : List Style
    , radioText : List Style
    , searchSettingsRow : List Style
    }


default : Graph
default =
    { root = []
    , addressFlags = []
    , abuseFlag = []
    , flagsGap = 5
    , addressLabel = []
    , addressRect = []
    , nodeFrame = \_ _ -> []
    , addressRoot = []
    , entityFlags = []
    , entityLabel = []
    , entityCurrency = []
    , entityAddressesCount = []
    , entityRect = []
    , entityRoot = []
    , link = \_ _ -> []
    , linkThickness = 1
    , linkColorFaded = Color.grey
    , linkColorStrong = Color.black
    , linkColorSelected = Color.red
    , linkLabel = \_ -> []
    , linkLabelBox = \_ -> []
    , expandHandle = always []
    , expandHandlePath = \_ _ -> []
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
    , searchTextarea = []
    , toolbox = \_ -> []
    , legendItem = []
    , legendItemColor = []
    , legendItemTitle = []
    , radio = []
    , radioInput = []
    , radioText = []
    , searchSettingsRow = []
    }
