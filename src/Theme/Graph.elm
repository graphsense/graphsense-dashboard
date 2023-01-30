module Theme.Graph exposing (Graph, default)

import Color exposing (Color)
import Css exposing (Style)
import Model.Graph exposing (NodeType)
import Model.Graph.Tool as Tool


type alias Graph =
    { root : List Style
    , addressFlags : List Style
    , abuseFlag : Bool -> List Style
    , flag : Bool -> List Style
    , flagsGap : Float
    , addressLabel : Bool -> List Style
    , addressRect : List Style
    , nodeFrame : Bool -> NodeType -> Bool -> List Style
    , addressRoot : List Style
    , entityFlags : List Style
    , entityLabel : Bool -> List Style
    , labelText : NodeType -> List Style
    , entityCurrency : Bool -> List Style
    , entityAddressesCount : Bool -> List Style
    , entityRect : List Style
    , entityRoot : List Style
    , link : Bool -> NodeType -> Bool -> Bool -> Maybe Color.Color -> List Style
    , linkThickness : Float
    , linkColorFaded : Bool -> Color
    , linkColorStrong : Bool -> Color
    , linkColorSelected : Bool -> Color
    , linkLabel : Bool -> Bool -> Bool -> Maybe Color.Color -> List Style
    , linkLabelBox : Bool -> Bool -> Bool -> List Style
    , shadowLink : Bool -> List Style
    , expandHandle : NodeType -> List Style
    , expandHandlePath : Bool -> NodeType -> Bool -> List Style
    , expandHandleText : Bool -> NodeType -> List Style
    , nodeSeparatorToExpandHandle : Bool -> NodeType -> List Style
    , graphRoot : List Style
    , svgRoot : Bool -> List Style
    , navbar : Bool -> List Style
    , navbarLeft : List Style
    , navbarRight : List Style
    , tool : Bool -> Tool.Status -> List Style
    , colorScheme : List Color
    , highlightsColorScheme : List Color
    , lightnessFactor :
        { entity : Float
        , address : Float
        }
    , saturationFactor :
        { entity : Float
        , address : Float
        }
    , defaultColor : Color
    , searchTextarea : Bool -> List Style
    , toolbox : Bool -> Bool -> List Style
    , legendItem : List Style
    , legendItemColor : List Style
    , legendItemTitle : List Style
    , radio : List Style
    , radioInput : List Style
    , radioText : List Style
    , searchSettingsRow : List Style
    , tagLockedIcon : List Style
    , tagLockedText : List Style
    , highlightsRoot : List Style
    , highlights : List Style
    , highlightsColors : List Style
    , highlightsColor : List Style
    , highlightRoot : List Style
    , highlightColor : Bool -> Bool -> List Style
    , highlightTitle : Bool -> List Style
    , highlightTrash : Bool -> List Style
    }


default : Graph
default =
    { root = []
    , addressFlags = []
    , abuseFlag = \_ -> []
    , flag = \_ -> []
    , flagsGap = 5
    , addressLabel = \_ -> []
    , addressRect = []
    , nodeFrame = \_ _ _ -> []
    , addressRoot = []
    , entityFlags = []
    , entityLabel = \_ -> []
    , labelText = \_ -> []
    , entityCurrency = \_ -> []
    , entityAddressesCount = \_ -> []
    , entityRect = []
    , entityRoot = []
    , link = \_ _ _ _ _ -> []
    , linkThickness = 1
    , linkColorFaded = \_ -> Color.grey
    , linkColorStrong = \_ -> Color.black
    , linkColorSelected = \_ -> Color.red
    , linkLabel = \_ _ _ _ -> []
    , linkLabelBox = \_ _ _ -> []
    , shadowLink = \_ -> []
    , expandHandle = always []
    , expandHandlePath = \_ _ _ -> []
    , expandHandleText = \_ _ -> []
    , nodeSeparatorToExpandHandle = \_ _ -> []
    , graphRoot = []
    , svgRoot = \_ -> []
    , navbar = \_ -> []
    , navbarLeft = []
    , navbarRight = []
    , tool = \_ _ -> []
    , colorScheme = []
    , highlightsColorScheme = []
    , lightnessFactor =
        { entity = 1
        , address = 1
        }
    , saturationFactor =
        { entity = 1
        , address = 1
        }
    , defaultColor = Color.rgb 255 255 255
    , searchTextarea = \_ -> []
    , toolbox = \_ _ -> []
    , legendItem = []
    , legendItemColor = []
    , legendItemTitle = []
    , radio = []
    , radioInput = []
    , radioText = []
    , searchSettingsRow = []
    , tagLockedIcon = []
    , tagLockedText = []
    , highlightsRoot = []
    , highlights = []
    , highlightsColors = []
    , highlightsColor = []
    , highlightRoot = []
    , highlightTitle = \_ -> []
    , highlightColor = \_ _ -> []
    , highlightTrash = \_ -> []
    }
