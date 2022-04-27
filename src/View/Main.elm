module View.Main exposing (main_)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Stats.View as Stats
import View.Config exposing (Config)


main_ :
    Config
    -> Model key
    -> Html Msg
main_ vc model =
    Stats.stats vc model.stats
