module View.Stats exposing (stats)

import Api.Data
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import Locale.Model as Locale
import Locale.View as Locale
import Model exposing (..)
import Msg exposing (..)
import RemoteData as RD exposing (WebData)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as Svg exposing (d, viewBox)
import Util.RemoteData exposing (webdata)
import View.Config exposing (Config)
import View.Css as Css
import View.Css.Stats as Css
import View.CurrencyMeta exposing (currencies)


stats : Config -> WebData Api.Data.Stats -> Html Msg
stats vc sts =
    div
        []
        [ h2
            [ Css.heading2 vc |> css
            ]
            [ Locale.text vc.locale "ledger statistics"
            ]
        , sts
            |> webdata
                { onFailure = statsLoadFailure vc
                , onNotAsked = text ""
                , onLoading = statsLoading vc
                , onSuccess = statsLoaded vc
                }
        ]


statsLoadFailure : Config -> Http.Error -> Html Msg
statsLoadFailure vc error =
    text "error"


statsLoading : Config -> Html Msg
statsLoading vc =
    text "loading"


statsLoaded : Config -> Api.Data.Stats -> Html Msg
statsLoaded vc sts =
    sts.currencies
        |> List.map (currency vc)
        |> div
            [ Css.root vc |> css
            ]


currency : Config -> Api.Data.CurrencyStats -> Html Msg
currency vc cs =
    div
        [ Css.currency vc |> css
        ]
        [ h3
            [ Css.currencyHeading vc |> css
            ]
            [ Dict.get cs.name currencies
                |> Maybe.map .name
                |> Maybe.withDefault (cs.name |> String.toUpper)
                |> text
            ]
        , div
            [ Css.statsTableWrapper vc |> css
            ]
            [ div
                [ Css.statsTableInnerWrapper vc |> css
                ]
                [ div
                    [ Css.statsTable vc |> css
                    ]
                    [ String.fromInt cs.timestamp
                        |> statsRow vc "Last update"
                    , String.fromInt (cs.noBlocks - 1)
                        |> statsRow vc "Latest block"
                    , String.fromInt cs.noTxs
                        |> statsRow vc "Transactions"
                    , String.fromInt cs.noAddresses
                        |> statsRow vc "Addresses"
                    , String.fromInt cs.noEntities
                        |> statsRow vc "Entities"
                    , String.fromInt cs.noLabels
                        |> statsRow vc "Labels"
                    , String.fromInt cs.noTaggedAddresses
                        |> statsRow vc "Tagged addresses"
                    ]
                ]
            , div
                [ Css.currencyBackground vc |> css
                ]
                [ Dict.get cs.name currencies
                    |> Maybe.map
                        (\{ icon } ->
                            svg
                                [ viewBox "0 0 100 100"
                                , attribute "height" "100%"
                                , attribute "width" "100%"
                                ]
                                [ path
                                    [ Css.currencyBackgroundPath vc |> Svg.css
                                    , d icon
                                    ]
                                    []
                                ]
                        )
                    |> Maybe.withDefault (span [] [])
                ]
            ]
        ]


statsRow : Config -> String -> String -> Html Msg
statsRow vc label value =
    div
        [ Css.statsTableRow vc |> css
        ]
        [ span
            [ Css.statsTableCellKey vc |> css
            ]
            [ Locale.text vc.locale label
            ]
        , span
            [ Css.statsTableCellValue vc |> css
            ]
            [ text value
            ]
        ]
