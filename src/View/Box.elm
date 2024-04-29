module View.Box exposing (box)

import Config.View as View
import Css.View as Css
import Html.Styled exposing (Attribute, Html, div)
import Html.Styled.Attributes exposing (css)


box : View.Config -> List (Attribute msg) -> List (Html msg) -> Html msg
box vc attr =
    div
        ((Css.box vc |> css) :: attr)
