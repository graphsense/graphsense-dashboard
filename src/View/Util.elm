module View.Util exposing (copyableLongIdentifier, longIdentifier)

import Config.View exposing (Config)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (style)
import Util.View exposing (truncateLongIdentifier)
import View.Button exposing (copyLink)


copyableLongIdentifier : Config -> String -> (String -> msg) -> Html msg
copyableLongIdentifier vc address effConst =
    span [ style "display" "inline-block", style "font-family" "monospace" ]
        [ text (truncateLongIdentifier address)
        , copyLink vc (effConst address)
        ]


longIdentifier : Config -> String -> Html msg
longIdentifier vc address =
    span [ style "display" "inline-block", style "font-family" "monospace" ] [ text (truncateLongIdentifier address) ]
