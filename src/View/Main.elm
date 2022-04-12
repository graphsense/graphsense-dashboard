module View.Main exposing (main_)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import View.Stats as Stats


main_ : Model key -> Html Msg
main_ model =
    Stats.stats model.locale model.stats
