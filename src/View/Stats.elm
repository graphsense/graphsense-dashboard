module View.Stats exposing (stats)

import Api.Data
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import Locale.Model as Locale
import Model exposing (..)
import Msg exposing (..)
import RemoteData as RD exposing (WebData)
import Util.RemoteData exposing (webdata)
import View.Css.Stats as Css
import View.Env exposing (Env)


stats : Env -> WebData Api.Data.Stats -> Html Msg
stats env sts =
    sts
        |> webdata
            { onFailure = statsLoadFailure env
            , onNotAsked = text ""
            , onLoading = statsLoading env
            , onSuccess = statsLoaded env
            }


statsLoadFailure : Env -> Http.Error -> Html Msg
statsLoadFailure env error =
    text "error"


statsLoading : Env -> Html Msg
statsLoading env =
    text "loading"


statsLoaded : Env -> Api.Data.Stats -> Html Msg
statsLoaded env sts =
    sts.currencies
        |> List.map (currency env)
        |> div
            [ Css.root env |> css
            ]


currency : Env -> Api.Data.CurrencyStats -> Html Msg
currency env cs =
    div
        [ Css.currency env |> css
        ]
        [ h2
            [ Css.currencyHeading env |> css
            ]
            [ String.toUpper cs.name
                |> text
            ]
        ]
