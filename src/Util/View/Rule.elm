module Util.View.Rule exposing (rule)

import Color
import Css exposing (..)
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css)
import Util.View exposing (toCssColor)


rule : Color.Color -> List (Attribute msg) -> List (Html msg) -> Html msg
rule col attributes =
    let
        ba =
            [ backgroundColor <| toCssColor col
            , Css.property "content" "\"\""
            , display inlineBlock
            , height <| px 1
            , position relative
            , verticalAlign middle
            , width <| pct 50
            ]
    in
    div <|
        [ css <|
            [ overflow hidden
            , textAlign center
            , before <|
                ba
                    ++ [ right <| em 0.5
                       , marginLeft <| pct -50
                       ]
            , after <|
                ba
                    ++ [ left <| em 0.5
                       , marginRight <| pct -50
                       ]
            ]
        ]
            ++ attributes
