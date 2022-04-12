module View exposing (view)

import Browser exposing (Document)
import Css exposing (..)
import Css.Reset
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Model exposing (..)
import Msg exposing (..)
import View.Header as Header
import View.Main as Main


view : Model key -> Document Msg
view model =
    { title = model.locale.getString "Iknaio Dashboard"
    , body =
        [ Css.Reset.meyerV2 |> toUnstyled
        , body model |> toUnstyled
        ]
    }


body : Model key -> Html Msg
body model =
    div
        [ css
            [ Css.height <| vh 100
            , displayFlex
            , flexDirection column
            , overflow Css.hidden
            ]
        ]
        [ Header.header
            { theme = model.theme
            , search = model.search
            , user = model.user
            }
        , section
            [ css
                [ displayFlex
                , flexDirection row
                , flexGrow (num 1)
                ]
            ]
            [ nav
                [ css
                    [ displayFlex
                    , flexDirection column
                    ]
                ]
                [ button
                    []
                    [ text "S"
                    ]
                , button
                    []
                    [ text "D"
                    ]
                ]
            , main_
                [ css
                    [ flexGrow (num 1)
                    ]
                ]
                [ Main.main_ model
                ]
            ]
        ]
