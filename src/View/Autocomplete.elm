module View.Autocomplete exposing (Config, dropdown)

import Config.View as View
import Css.Autocomplete as Css
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Util.View exposing (loadingSpinner)


type alias Config msg =
    { loading : Bool
    , visible : Bool
    , onClick : msg
    }


dropdown : View.Config -> Config msg -> List (Html msg) -> Html msg
dropdown vc config content =
    div
        [ Css.frame vc |> css
        ]
        [ if not config.visible || not config.loading && List.isEmpty content then
            span [] []

          else
            div
                [ css (Css.result vc)
                , onClick config.onClick
                ]
                ((if config.loading then
                    [ loadingSpinner vc Css.loadingSpinner ]

                  else
                    []
                 )
                    ++ content
                )
        ]
