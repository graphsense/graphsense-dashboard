module View.Autocomplete exposing (Config, dropdown, dropdownStyled)

import Config.View as View
import Css exposing (Style)
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


type alias Styles msg =
    { frame : List Style
    , result : List Style
    , loadingSpinner : Html msg
    }


dropdown : View.Config -> Config msg -> List (Html msg) -> Html msg
dropdown vc =
    dropdownStyled
        { frame = []
        , result = []
        , loadingSpinner = loadingSpinner vc Css.loadingSpinner
        }
        vc


dropdownStyled : Styles msg -> View.Config -> Config msg -> List (Html msg) -> Html msg
dropdownStyled styles vc config content =
    div
        [ Css.frame vc ++ styles.frame |> css
        ]
        [ if not config.visible || not config.loading && List.isEmpty content then
            span [] []

          else
            div
                [ css (Css.result vc ++ styles.result)
                , onClick config.onClick
                ]
                ((if config.loading then
                    [ styles.loadingSpinner ]

                  else
                    []
                 )
                    ++ content
                )
        ]
