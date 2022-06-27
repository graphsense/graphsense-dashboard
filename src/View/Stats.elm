module View.Stats exposing (stats)

import Api.Data
import Config.View exposing (Config)
import Css.Stats as Css
import Css.View
import Dict
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Http
import Model.Locale as Locale
import RemoteData as RD exposing (WebData)
import Svg.Styled exposing (path, svg)
import Svg.Styled.Attributes as Svg exposing (d, viewBox)
import Util.RemoteData exposing (webdata)
import Util.View
import View.CurrencyMeta exposing (currencies)
import View.Locale as Locale


stats : Config -> WebData Api.Data.Stats -> Html msg
stats vc sts =
    div
        [ Css.root vc |> css ]
        [ h2
            [ Css.View.heading2 vc |> css
            ]
            [ Locale.text vc.locale "Ledger statistics"
            ]
        , sts
            |> webdata
                { onFailure = statsLoadFailure vc
                , onNotAsked = text ""
                , onLoading = statsLoading vc
                , onSuccess = statsLoaded vc
                }
        ]


statsLoadFailure : Config -> Http.Error -> Html msg
statsLoadFailure vc error =
    text "error"


statsLoading : Config -> Html msg
statsLoading vc =
    Util.View.loadingSpinner vc Css.loadingSpinner


statsLoaded : Config -> Api.Data.Stats -> Html msg
statsLoaded vc sts =
    sts.currencies
        |> List.map (currency vc)
        |> div
            [ Css.stats vc |> css ]


currency : Config -> Api.Data.CurrencyStats -> Html msg
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
                    [ Locale.timestamp vc.locale cs.timestamp
                        |> statsRow vc "Last update"
                    , Locale.int vc.locale (cs.noBlocks - 1)
                        |> statsRow vc "Latest block"
                    , Locale.int vc.locale cs.noTxs
                        |> statsRow vc "Transactions"
                    , Locale.int vc.locale cs.noAddresses
                        |> statsRow vc "Addresses"
                    , Locale.int vc.locale cs.noEntities
                        |> statsRow vc "Entities"
                    , Locale.int vc.locale cs.noLabels
                        |> statsRow vc "Labels"
                    , taggedAddressesWithPercentage vc cs
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


statsRow : Config -> String -> String -> Html msg
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


taggedAddressesWithPercentage : Config -> Api.Data.CurrencyStats -> String
taggedAddressesWithPercentage vc cs =
    Locale.int vc.locale cs.noTaggedAddresses
        ++ " ("
        ++ Locale.percentage vc.locale
            (toFloat cs.noTaggedAddresses / toFloat cs.noAddresses)
        ++ ")"
