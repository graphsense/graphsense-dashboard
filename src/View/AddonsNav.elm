module View.AddonsNav exposing (nav)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Msg exposing (Msg)
import View.Config exposing (Config)


nav : Config -> Html Msg
nav config =
    Html.Styled.nav
        [ css
            ([ displayFlex
             , flexDirection column
             ]
                ++ config.theme.addonsNav
            )
        ]
        []
