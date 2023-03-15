module View.Util exposing (copyableLongIdentifier, longIdentifier)

import Config.View exposing (Config)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Util.View exposing (truncateLongIdentifier)
import View.Button exposing (copyLink)
import Css.Browser as BCss

copyableLongIdentifier : Config -> String -> (String -> msg) -> Html msg
copyableLongIdentifier vc address effConst =
    span [BCss.propertyLongIdentifier vc |> css]
        [ text (truncateLongIdentifier address)
        , copyLink vc (effConst address)
        ]


longIdentifier : Config -> String -> Html msg
longIdentifier vc address =
    span [BCss.propertyLongIdentifier vc |> css] [ text (truncateLongIdentifier address) ]
