module View.Autocomplete exposing (Config, Styles, dropdown, dropdownStyled)

import Config.View as View
import Css exposing (Style)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import RecordSetter as Rs
import Theme.Html.SearchComponents as SearchComponents
import Util.View exposing (fullWidthCss)
import Util.View.Loadingspinner as Loadingspinner


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
        , loadingSpinner =
            Loadingspinner.html
                [ css
                    [ Css.position Css.absolute
                    , Css.top Css.zero
                    , Css.right Css.zero
                    ]
                ]
        }
        vc


dropdownStyled : Styles msg -> View.Config -> Config msg -> List (Html msg) -> Html msg
dropdownStyled styles _ config content =
    div
        [ [ Css.overflow Css.visible, Css.position Css.relative ] |> css
        ]
        [ if not config.visible || not config.loading && List.isEmpty content then
            span [] []

          else
            SearchComponents.autocompleteWithAttributes
                (SearchComponents.autocompleteAttributes
                    |> Rs.s_root
                        [ onClick config.onClick
                        , css
                            [ Css.position Css.absolute
                            , Css.zIndex <| Css.int 200
                            , fullWidthCss
                            ]
                        ]
                )
                { autocompleteGroupList =
                    (if config.loading then
                        [ styles.loadingSpinner ]

                     else
                        []
                    )
                        ++ content
                }
                {}
        ]
