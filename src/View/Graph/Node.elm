module View.Graph.Node exposing (ExpandConfig, expand)

import Config.Graph as Graph exposing (expandHandleWidth)
import Config.View exposing (Config)
import Css
import Css.Graph as Css
import Json.Decode
import Model.Node exposing (NodeType)
import Msg.Graph exposing (Msg)
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Events
import Util.Graph exposing (translate)
import View.Locale as Locale


type alias ExpandConfig =
    { nodeType : NodeType
    , degree : Int
    , isOutgoing : Bool
    , width : Float
    , height : Float
    , onClick : Bool -> Msg
    , color : Css.Color
    , isSelected : Bool
    }


expand : Config -> Graph.Config -> ExpandConfig -> Svg Msg
expand vc gc { nodeType, degree, isOutgoing, width, height, onClick, color, isSelected } =
    g
        [ Css.expandHandle vc nodeType |> css
        , Events.stopPropagationOn "click" (Json.Decode.succeed ( onClick isOutgoing, True ))
        , translate
            (if isOutgoing then
                width + expandHandleWidth

             else
                expandHandleWidth
            )
            0
            ++ " rotate("
            ++ (if isOutgoing then
                    "0"

                else
                    "180"
                        ++ " 0 "
                        ++ String.fromFloat (height / 2)
               )
            ++ ")"
            |> transform
        ]
        [ Svg.path
            [ Css.expandHandlePath vc nodeType isSelected
                ++ [ Css.fill color ]
                |> css
            , String.Interpolate.interpolate
                "M0 0 C {0} 0, {0} 0, {0} {0} L {0} {1} C {0} {2} {0} {2} 0 {2}"
                (List.map String.fromFloat
                    [ expandHandleWidth
                    , height - expandHandleWidth
                    , height
                    ]
                )
                |> d
            ]
            []
        , let
            fs =
                expandHandleWidth * 0.8

            fmt =
                if degree > 99999 then
                    "1,000.0 a"

                else
                    "1,000"
          in
          text_
            [ textAnchor "middle"
            , (fs
                |> Css.px
                |> Css.fontSize
              )
                :: Css.expandHandleText vc nodeType
                |> css
            , height
                / 2
                |> translate (expandHandleWidth - fs)
                |> Util.Graph.rotate 90
                |> transform
            ]
            [ Locale.intWithFormat vc.locale fmt degree |> text
            ]
        ]
