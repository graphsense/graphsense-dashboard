module View.Graph.Node exposing (..)

import Config.Graph as Graph exposing (AddressLabelType(..), addressesCountHeight, expandHandleWidth, labelHeight)
import Config.View exposing (Config)
import Css exposing (fill)
import Css.Graph as Css
import Model.Graph exposing (NodeType(..))
import Msg.Graph exposing (Msg(..))
import String.Interpolate
import Svg.Styled as Svg exposing (..)
import Svg.Styled.Attributes exposing (..)
import Svg.Styled.Events as Events exposing (..)
import Util.Graph exposing (rotate, translate)
import View.Locale as Locale


type alias ExpandConfig =
    { nodeType : NodeType
    , degree : Int
    , isOutgoing : Bool
    , width : Float
    , height : Float
    , onClick : Bool -> Msg
    , color : Css.Color
    }


expand : Config -> Graph.Config -> ExpandConfig -> Svg Msg
expand vc gc { nodeType, degree, isOutgoing, width, height, onClick, color } =
    g
        [ Css.expandHandle vc nodeType |> css
        , Events.onClick (onClick isOutgoing)
        , translate
            (if isOutgoing then
                width

             else
                0
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
            [ Css.expandHandlePath vc nodeType
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
