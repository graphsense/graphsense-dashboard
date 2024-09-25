module Util.View.Rule exposing (rule)

import Color exposing (Color)
import Css
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css)
import Util.View exposing (toCssColor)


rule : Color -> List (Attribute msg) -> List (Html msg) -> Html msg
rule col attributes =
    let
        ba =
            [ Css.backgroundColor <| toCssColor col
            , Css.property "content" "\"\""
            , Css.display Css.inlineBlock
            , Css.height <| Css.px 1
            , Css.position Css.relative
            , Css.verticalAlign Css.middle
            , Css.width <| Css.pct 50
            ]
    in
    div <|
        (css <|
            [ Css.overflow Css.hidden
            , Css.textAlign Css.center
            , Css.before <|
                ba
                    ++ [ Css.right <| Css.em 0.5
                       , Css.marginLeft <| Css.pct -50
                       ]
            , Css.after <|
                ba
                    ++ [ Css.left <| Css.em 0.5
                       , Css.marginRight <| Css.pct -50
                       ]
            ]
        )
            :: attributes
