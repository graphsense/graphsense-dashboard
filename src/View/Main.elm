module View.Main exposing (main_)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import View.Env exposing (Env)
import View.Stats as Stats


main_ : Env -> Model key -> Html Msg
main_ env model =
    Stats.stats env model.stats
