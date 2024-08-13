module View.Pathfinder.Utils exposing (multiLineDateTimeFromTimestamp)

import Config.View as View
import Css
import Css.Pathfinder as PCSS exposing (inoutStyle, toAttr)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import View.Locale as Locale


multiLineDateTimeFromTimestamp : View.Config -> Int -> Html msg
multiLineDateTimeFromTimestamp vc d =
    let
        date =
            Locale.timestampDateUniform vc.locale d

        time =
            Locale.time vc.locale d
    in
    div []
        [ div [ [ PCSS.sGap |> Css.paddingBottom ] |> css ] [ text date ]
        , div [ [ PCSS.sText |> Css.fontSize ] |> css ] [ text time ]
        ]
